module Main where

import Prelude
import Data.Tuple
import Data.Either
import Data.Version (Version(..), parseVersion)
import Data.String (split, trim)
import Control.Monad.Aff
import Control.Monad.Aff.Console
import Network.HTTP.Affjax (affjax, defaultRequest)
import Network.HTTP.RequestHeader (RequestHeader(Accept))
import Network.HTTP.MimeType (MimeType(..))

main = launchAff do
  pkgList <- getPackageList
  startFrom <- traverse (\pkg -> Tuple pkg <$> getEarliestVersion pkg) pkgList

getPackageList :: Aff _ (Array String)
getPackageList = do
  resp <- affjax $ defaultRequest
                    { url     = "http://pursuit.purescript.org/packages"
                    , headers = [ Accept (MimeType "text/plain") ]
                    }
  (map trim <<< splitOn "\n") <$> resp.response

type AvailableVersions = Array (Tuple String Version)

getEarliestVersion pkg = do
  resp <- getAvailableVersions pkg
  pure (fst vers)

getAvailableVersions :: String -> Aff _ AvailableVersions
getAvailableVersions pkg = do
  resp <- affjax $ defaultRequest
                    { url     = availableVersionsUrl pkg
                    , headers = [ Accept (MimeType "application/json") ]
                    }

  pure (resp.response)
  where
  availableVersionsUrl p =
    "http://pursuit.purescript.org/packages/" <> p <> "/available-versions"
