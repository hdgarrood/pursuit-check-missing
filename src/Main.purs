module Main where

import Prelude
import Control.Apply
import Data.Tuple
import Data.Either
import Data.Maybe
import Data.Version (Version(), parseVersion, showVersion)
import Data.String (split, trim)
import Data.Array (head, sort, catMaybes, take, length)
import Data.Foldable
import Data.Traversable
import Data.Foreign
import Data.Foreign.Class
import Control.Monad.Error.Class (throwError)
import qualified Control.Monad.Eff.Exception as Exception
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff
import Control.Monad.Aff.Par
import Control.Monad.Aff.Console
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(Accept))
import Network.HTTP.MimeType (MimeType(..))

main = launchAff do
  pkgList <- getPackageList
  error (show (length pkgList) <> " packages to process")

  packages <- traverse (\pkg -> Tuple pkg <$> getAvailableVersions pkg) pkgList

  for_ packages $ \(Tuple pkg vers) ->
    error $ pkg <> ": " <> show (map showVersion vers)



getPackageList :: Aff _ (Array String)
getPackageList = do
  resp <- affjax $ defaultRequest
                    { url     = "http://pursuit.purescript.org/packages"
                    , headers = [ Accept (MimeType "application/json") ]
                    }
  readOrThrow resp.response

getEarliestVersion :: String -> Aff _ Version
getEarliestVersion pkg = do
  vers <- getAvailableVersions pkg
  case head (sort vers) of
    Just v -> pure v
    Nothing -> err ("No versions found for " <> pkg)

getAvailableVersions :: String -> Aff _ (Array Version)
getAvailableVersions pkg = do
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

err :: forall a. String -> Aff _ a
err = throwError <<< Exception.error

readOrThrow :: forall a. (IsForeign a) => Foreign -> Aff _ a
readOrThrow = either (err <<< ("readOrThrow: " <>) <<< show) pure <<< read
