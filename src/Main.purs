module Main where

import Prelude
import Control.Apply
import Control.Monad
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Version (Version(), parseVersion, showVersion)
import Data.String (split, trim)
import Data.Array (head, sort, catMaybes, take, length, null, (\\), filter)
import Data.Foldable
import Data.Traversable
import Data.Foreign
import Data.Foreign.Class
import Control.Monad.Error.Class (throwError)
import Control.Monad.Eff
import Control.Monad.Eff.Class (liftEff)
import qualified Control.Monad.Eff.Exception as Exception
import qualified Control.Monad.Eff.Console as EffConsole
import Control.Monad.Aff
import Control.Monad.Aff.AVar
import Control.Monad.Aff.Par
import Control.Monad.Aff.Console
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(Accept))
import Network.HTTP.MimeType (MimeType(..))
import Unsafe.Coerce

type PackageDetails =
  { name :: String
  , pursuitVersions :: Array Version
  , bowerVersions :: Array Version
  }

main = launchAff do
  pkgList <- getPackageList
  error (show (length pkgList) <> " packages to process")

  packages <- traverse getPursuitAndBowerVersions pkgList

  packages' <- traverse (\pkg -> Tuple pkg <$> checkDetails pkg) packages

  log (unsafeCoerce packages')

getPackageList :: Aff _ (Array String)
getPackageList = do
  resp <- affjax $ defaultRequest
                    { url     = "http://pursuit.purescript.org/packages"
                    , headers = [ Accept (MimeType "application/json") ]
                    }
  readOrThrow resp.response

getPursuitAndBowerVersions :: String -> Aff _ PackageDetails
getPursuitAndBowerVersions pkg = do
  Tuple pursuit bower <- runPar (Tuple <$> Par (getPursuitVersions pkg)
                                       <*> Par (getBowerVersions pkg))
  when (null pursuit)
    (err (pkg <> " had no available versions on pursuit."))
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
    "http://pursuit.purescript.org/packages/" <> p <> "/available-versions"

  parse arr =
    case arr of
      [v, _] ->
        case parseVersion v of
          Right v' -> pure (Just v')
          Left _   -> error (pkg <> ": failed to parse version: " <> v) *> pure Nothing
      _ ->
        err ("Array was the wrong shape (expected two elements): " <> show arr)

getBowerVersions :: String -> Aff _ (Array Version)
getBowerVersions pkg = do
  res <- runProcess "bower" ["info", pkg, "--json"]
  versions <- rightOrThrow (parseJSON res.stdout >>= readProp "versions")
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

foreign import runProcessEff :: forall e.
  String
  -> Array String
  -> (Exception.Error -> Eff e Unit)
  -> ({ stdout :: String, stderr :: String } -> Eff e Unit)
  -> Eff e Unit

-- | Run a process and get stdout and stderr.
runProcess :: String -> Array String -> Aff _ { stdout :: String, stderr :: String }
runProcess cmd args = makeAff (runProcessEff cmd args)

err :: forall a. String -> Aff _ a
err = throwError <<< Exception.error

rightOrThrow :: forall a b. (Show a) => Either a b -> Aff _ b
rightOrThrow = either (err <<< ("rightOrThrow: " <>) <<< show) pure

readOrThrow :: forall a. (IsForeign a) => Foreign -> Aff _ a
readOrThrow = rightOrThrow <<< read

error :: forall e. String -> Aff (console :: EffConsole.CONSOLE | e) Unit
error = liftEff <<< EffConsole.error
