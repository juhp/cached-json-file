{-# LANGUAGE OverloadedStrings #-}

module System.Cached.JSON (
  getCachedJSON
  )
where

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Time.Clock (diffUTCTime, getCurrentTime, NominalDiffTime)
import Network.HTTP.Query
import System.Directory
import System.Environment.XDG.BaseDir
import System.FilePath

-- FIXME handle network failure
getCachedJSON :: String -- ^ program name
              -> FilePath -- ^ filename
              -> String -- ^ json url
              -> Query -- ^ url query params list
              -> NominalDiffTime -- ^ cache duration (minutes)
              -> IO Object
getCachedJSON prog jsonfile url params minutes = do
  file <- getUserCacheFile prog jsonfile
  exists <- doesFileExist file
  unless exists $ do
    putStrLn $ "Creating " ++ file ++ " ..."
    createDirectoryIfMissing True (takeDirectory file)
  recent <- do
    if exists then do
      ts <- getModificationTime file
      t <- getCurrentTime
      return $ diffUTCTime t ts < (minutes * 60)
      else return False
  if recent
    then do
    eObj <- eitherDecode <$> B.readFile file
    case eObj of
      Left err -> error err
      Right obj -> return obj
    else do
    obj <- webAPIQuery url params
    B.writeFile file $ encode obj
    return obj
