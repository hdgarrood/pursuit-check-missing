module Main where

import Prelude
import Control.Apply
import Control.Bind
import Control.Monad
import Control.Alt
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Version (Version(), parseVersion, showVersion)
import Data.String (split, trim)
import Data.Array (head, sort, catMaybes, take, length, null, (\\), filter)
import Data.Array as Array
import Data.StrMap as StrMap
import Data.Foldable
import Data.Traversable
import Data.Bifunctor (lmap, rmap)
import Data.Foreign
import Data.Foreign.Class
import Control.Monad.Error.Class
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION())
import Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Console as EffConsole
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Aff.Par
import Control.Monad.Aff.Console
import Global.Unsafe (unsafeStringify)
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.RequestHeader
import Network.HTTP.MimeType (MimeType(..))
import Network.HTTP.Method
import Network.HTTP.StatusCode
import Node.ChildProcess as ChildProcess
import Node.Process (PROCESS())
import Node.Process as Process
import Node.Encoding
import Node.Buffer as Buffer
import Node.FS.Aff
import Unsafe.Coerce (unsafeCoerce)

main = runAff (EffConsole.error <<< show) (const (Process.exit 0)) do
 submitAllBowerVersionsFromFile "missing-completely.txt"

-- Given a plain text file of package names, attempt to submit all registered
-- bower versions of each package in the file.
submitAllBowerVersionsFromFile filepath = do
  pkgNames <- split "\n" <$> readTextFile UTF8 filepath
  log (show (Array.length pkgNames) <> " packages to process")
  pkgs <- traverse (\pkg -> Tuple pkg <$> getBowerVersions pkg) pkgNames
  submitAll pkgs

-- | Useful for recording all the packages which are on pursuit and their
-- | versions.
jsonEncodePursuitPackages :: Array (Tuple String (Array Version)) -> String
jsonEncodePursuitPackages = unsafeStringify <<< map encode
  where
  encode :: Tuple String (Array Version) -> Foreign
  encode (Tuple pkg versions) =
    toForeign $
      StrMap.fromFoldable
        [ Tuple "packageName" (toForeign pkg)
        , Tuple "versions" (toForeign (map showVersion versions))
        ]

jsonDecodePursuitPackages :: String -> F (Array (Tuple String (Array Version)))
jsonDecodePursuitPackages = parseJSON >=> readArray >=> traverse go
  where
  go :: Foreign -> F (Tuple String (Array Version))
  go obj = do
    pkgName <- readProp "packageName" obj
    versions <- readProp "versions" obj
                  >>= traverse (parseVersion >>> lmap (show >>> JSONError))
    pure (Tuple pkgName versions)

-- Get details of all packages in pursuit and print to stdout. This was part 1
-- of how we migrated the pursuit data to the new format after psc 0.8 was
-- released (which changed the serialization format).
logAllPursuitPackages = do
  pursuitPkgs <- getAllPursuitPackages
  log (jsonEncodePursuitPackages pursuitPkgs)

-- Get all pursuit packages with their versions.
getAllPursuitPackages :: Aff _ (Array (Tuple String (Array Version)))
getAllPursuitPackages = do
  pkgList <- getPackageList
  traverse (\pkg -> Tuple pkg <$> getPursuitVersions pkg) pkgList

-- Restore from a `logAllPursuitPackages`. Part 2 of the 0.7->0.8 migration
-- (see logAllPursuitPackages)
restorePursuitPackages = do
  json <- readTextFile UTF8 "out.json"
  pkgs <- rightOrThrow (jsonDecodePursuitPackages json)
  submitAll pkgs

submitAll :: Array (Tuple String (Array Version)) -> Aff _ Unit
submitAll pkgs = do
  for_ pkgs $ \(Tuple pkg versions) ->
    for_ versions $ \version -> do
      error $ "Attempting to submit " <> pkg <> " at " <> showVersion version
      flip catchError (submitErr pkg version) $ trySubmit pkg version

  where
  submitErr :: String -> Version -> Exception.Error -> Aff _ Unit
  submitErr pkg v e = do
    error $ "Error while trying to submit " <> show pkg <> " at " <> showVersion v <> ":"
    error $ unsafeCoerce e

-- Compare published versions on pursuit and bower, and add any newer released
-- versions of packages which are on Bower, but have not yet been uploaded to
-- Pursuit.
fixMissing = do
  pkgList <- getPackageList
  error (show (length pkgList) <> " packages to process")

  missing <- traverse (\pkg -> Tuple pkg <$> getMissing pkg) pkgList

  -- write missing package details to stdout
  -- log (missingToJSON missing)

  submitAll missing

type PackageDetails =
  { name :: String
  , pursuitVersions :: Array Version
  , bowerVersions :: Array Version
  }

getPackageList :: Aff _ (Array String)
getPackageList = do
  resp <- affjax $ defaultRequest
                    { url     = "https://pursuit.purescript.org/packages"
                    , headers = [ Accept (MimeType "application/json") ]
                    }
  readOrThrow resp.response

getMissing :: String -> Aff _ (Array Version)
getMissing = getPursuitAndBowerVersions >=> checkDetails

missingToJSON :: Array (Tuple String (Array Version)) -> String
missingToJSON = map (\t -> { name: fst t, missing: map showVersion (snd t) }) >>> unsafeStringify

getPursuitAndBowerVersions :: String -> Aff _ PackageDetails
getPursuitAndBowerVersions pkg = do
  pursuit <- getPursuitVersions pkg
  when (null pursuit)
    (err (pkg <> " had no available versions on pursuit."))
  bower <- getBowerVersions pkg
  pure $ { name: pkg, pursuitVersions: pursuit, bowerVersions: bower }

getPursuitVersions :: String -> Aff _ (Array Version)
getPursuitVersions pkg = do
  resp <- affjax $ defaultRequest
                    { url     = availableVersionsUrl pkg
                    , headers = [ Accept (MimeType "application/json") ]
                    }
  arr <- readOrThrow resp.response
  error ("getting available versions: " <> pkg)
  catMaybes <$> traverse parse arr

  where
  availableVersionsUrl p =
    "https://pursuit.purescript.org/packages/" <> p <> "/available-versions"

  parse arr =
    case arr of
      [v, _] ->
        case parseVersion v of
          Right v' -> pure (Just v')
          Left _   -> error (pkg <> ": failed to parse version: " <> v) *> pure Nothing
      _ ->
        err ("Array was the wrong shape (expected two elements): " <> show arr)

getBowerVersions :: String -> Aff _ (Array Version)
getBowerVersions pkg = flip catchError (\(_ :: Exception.Error) -> pure []) do
  json <- run "bower" ["info", pkg, "--json"]
  versions <- rightOrThrow (parseJSON json) >>= readProp' "versions"
  rightOrThrow $ traverse parseVersion versions

checkDetails :: PackageDetails -> Aff _ (Array Version)
checkDetails pkg = do
  earliest <- rightOrThrow (min pkg.pursuitVersions)
  let bowers = filter (>= earliest) pkg.bowerVersions
  pure $ bowers \\ pkg.pursuitVersions
  where
  min arr =
    case head (sort arr) of
      Just h -> Right h
      Nothing -> Left unit

trySubmit :: String -> Version -> Aff _ Unit
trySubmit pkg vers = do
  repo <- getRepository pkg vers
  let tmpdir = getTmpDir pkg vers
  doesExist <- exists tmpdir
  when doesExist
    (void (run "rm" ["-r", tmpdir]))
  _ <- gitClone repo tmpdir
  cd tmpdir
  _ <- gitCheckout (showVersion vers) <|> gitCheckout ("v" <> showVersion vers)
  _ <- bowerInstall
  json <- pscPublish
  home <- envHome
  token <- readTextFile UTF8 (home <> "/.pulp/github-oauth-token")
  pursuitSubmit token json
  where
  getTmpDir pkg vers = "/tmp/pursuit-check-missing-" <> pkg <> "/" <> showVersion vers
  gitClone repo dir = run "git" ["clone", repo, dir]
  gitCheckout tag = run "git" ["checkout", tag]
  bowerInstall = run "bower" ["install"]
  pscPublish = run "psc-publish" []
  pursuitSubmit token json = do
    r <- affjax $ defaultRequest
          { url = "https://pursuit.purescript.org/packages"
          , method = POST
          , headers = [ Accept (MimeType "application/json")
                      , RequestHeader "Authorization" ("token " <> token)
                      ]
          , content = Just json
          }
    when (r.status /= StatusCode 201) do
      error $ "Status: " <> show r.status
      error $ show r.headers
      error $ r.response
      err "submit failed"
    pure unit


getRepository :: String -> Version -> Aff _ String
getRepository pkg vers = do
  json <- run "bower" ["info", pkg <> "#" <> showVersion vers, "--json"]
  rightOrThrow (parseJSON json) >>= readProp' "repository" >>= readProp' "url"

-- | Run a command and args, and get stdout.
run :: String -> Array String -> Aff _ String
run cmd args =
  makeAff \err' done ->
    ChildProcess.execFile cmd args opts \r ->
      case r.error of
        Just e -> err' e
        Nothing -> Buffer.toString UTF8 r.stdout >>= done
  where
  opts = ChildProcess.defaultExecOptions { maxBuffer = Just fiveMegs }
  fiveMegs = 1024 * 1024 * 5

cd :: forall e. String -> Aff (process :: PROCESS, err :: EXCEPTION | e) Unit
cd dir = liftEff (Process.chdir dir)

envHome :: forall e. Aff (process :: PROCESS | e) String
envHome =
  liftEff (Process.lookupEnv "HOME")
    >>= maybe (err "HOME env var is not defined") pure

err :: forall a. String -> Aff _ a
err = throwError <<< Exception.error

rightOrThrow :: forall a b. (Show a) => Either a b -> Aff _ b
rightOrThrow = either (err <<< ("rightOrThrow: " <>) <<< show) pure

readOrThrow :: forall a. (IsForeign a) => Foreign -> Aff _ a
readOrThrow = rightOrThrow <<< read

readProp' :: forall a. (IsForeign a) => String -> Foreign -> Aff _ a
readProp' prop f =
  case readProp prop f of
    Right val -> pure val
    Left e -> err ("readProp': failed to read " <> prop <> ": " <> show e)

error :: forall e. String -> Aff (console :: EffConsole.CONSOLE | e) Unit
error = liftEff <<< EffConsole.error
