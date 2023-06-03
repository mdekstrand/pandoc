{-# LANGUAGE CPP #-}
{-# LANGUAGE ScopedTypeVariables #-}
#ifdef EMBED_DATA_FILES
{-# LANGUAGE TemplateHaskell #-}
#endif
{- |
Module      : Text.Pandoc.Data
Copyright   : Copyright (C) 2013-2023 John MacFarlane
License     : GNU GPL, version 2 or above

Maintainer  : John MacFarlane <jgm@berkeley@edu>
Stability   : alpha
Portability : portable

Access to pandoc's data files.
-}
module Text.Pandoc.Data ( readDefaultDataFile
                        , readDataFile
                        , getDataFileNames
                        , defaultUserDataDir
                        ) where
import Text.Pandoc.Class (PandocMonad(..), checkUserDataDir)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Control.Monad.Except (throwError)
import Text.Pandoc.Error (PandocError(..))
import System.FilePath
import System.Directory
import qualified Control.Exception as E
#ifdef EMBED_DATA_FILES
import Text.Pandoc.Data.BakedIn (dataFiles)
import Text.Pandoc.Shared (makeCanonical)
#else
import Paths_pandoc_lite (getDataDir)
#endif

-- | Read file from from the default data files.
readDefaultDataFile :: PandocMonad m => FilePath -> m B.ByteString
readDefaultDataFile fname =
#ifdef EMBED_DATA_FILES
  case lookup (makeCanonical fname) dataFiles of
    Nothing       -> throwError $ PandocCouldNotFindDataFileError $ T.pack fname
    Just contents -> return contents
#else
  getDataFileName fname' >>= checkExistence >>= readFileStrict
    where fname' = if fname == "MANUAL.txt" then fname else "data" </> fname

-- | Returns the input filename unchanged if the file exits, and throws
-- a `PandocCouldNotFindDataFileError` if it doesn't.
checkExistence :: PandocMonad m => FilePath -> m FilePath
checkExistence fn = do
  exists <- fileExists fn
  if exists
     then return fn
     else throwError $ PandocCouldNotFindDataFileError $ T.pack fn
#endif

--- | Read file from user data directory or,
--- if not found there, from the default data files.
readDataFile :: PandocMonad m => FilePath -> m B.ByteString
readDataFile fname = do
  datadir <- checkUserDataDir fname
  case datadir of
       Nothing -> readDefaultDataFile fname
       Just userDir -> do
         exists <- fileExists (userDir </> fname)
         if exists
            then readFileStrict (userDir </> fname)
            else readDefaultDataFile fname

getDataFileNames :: IO [FilePath]
getDataFileNames = do
#ifdef EMBED_DATA_FILES
  let allDataFiles = map fst dataFiles
#else
  allDataFiles <- filter (\x -> x /= "." && x /= "..") <$>
                      (getDataDir >>= getDirectoryContents)
#endif
  return $ "reference.docx" : "reference.odt" : "reference.pptx" : allDataFiles

-- | Return appropriate user data directory for platform.  We use
-- XDG_DATA_HOME (or its default value), but for backwards compatibility,
-- we fall back to the legacy user data directory ($HOME/.pandoc on *nix)
-- if the XDG_DATA_HOME is missing and this exists.  If neither directory
-- is present, we return the XDG data directory.  If the XDG data directory
-- is not defined (e.g. because we are in an environment where $HOME is
-- not defined), we return the empty string.
defaultUserDataDir :: IO FilePath
defaultUserDataDir = do
  xdgDir <- E.catch (getXdgDirectory XdgData "pandoc")
               (\(_ :: E.SomeException) -> return mempty)
  xdgExists <- doesDirectoryExist xdgDir
  legacyDir <- E.catch (getAppUserDataDirectory "pandoc")
                (\(_ :: E.SomeException) -> return mempty)
  legacyDirExists <- doesDirectoryExist legacyDir
  if not xdgExists && legacyDirExists
     then return legacyDir
     else return xdgDir
