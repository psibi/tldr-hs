{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns    #-}

module Tldr.App.Handler
  ( handleAboutFlag
  , retriveLocale
  , checkLocale
  , englishViewOptions
  , getCheckDirs
  , pageExists
  , getPagePath
  , updateTldrPages
  , handleTldrOpts
  ) where

import Data.Char (toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.Version (showVersion)
import Data.Time.Clock
import Control.Monad (when)
import Options.Applicative
import Paths_tldr (version)
import System.Directory
  ( XdgDirectory(..)
  , createDirectory
  , removePathForcibly
  , doesFileExist
  , doesDirectoryExist
  , getModificationTime
  , getXdgDirectory
  )
import System.Environment (lookupEnv, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr, stdout)
import Network.HTTP.Simple
import Codec.Archive.Zip
import Tldr
import Tldr.App.Constant
import Tldr.Types

handleAboutFlag :: IO ()
handleAboutFlag = do
  path <- getExecutablePath
  let content =
        unlines
          [ path <> " v" <> showVersion version
          , "Copyright (C) 2017 Sibi Prabakaran"
          , "Source available at https://github.com/psibi/tldr-hs"
          ]
  putStr content

retriveLocale :: IO Locale
retriveLocale = do
  lang <- lookupEnv "LANG"
  pure $ computeLocale lang

checkLocale :: Locale -> Bool
checkLocale English = True
checkLocale _ = False

englishViewOptions :: ViewOptions -> ViewOptions
englishViewOptions xs = xs { languageOption = Just "en_US.utf8" }

handleTldrOpts :: TldrOpts -> IO ()
handleTldrOpts opts@TldrOpts {..} =
  case tldrAction of
    UpdateIndex -> updateTldrPages
    About -> handleAboutFlag
    ViewPage voptions pages -> do
      shouldPerformUpdate <- updateNecessary opts
      when shouldPerformUpdate updateTldrPages
      let npage = intercalate "-" pages
      locale <-
        case languageOption voptions of
          Nothing -> retriveLocale
          Just lg -> pure $ computeLocale (Just lg)
      fname <- getPagePath locale npage (getCheckDirs voptions)
      case fname of
        Just path -> do
          defColor <- getNoColorEnv
          let color = fromMaybe defColor colorSetting
          renderPage path stdout color
        Nothing ->
          if checkLocale locale
            then do
              hPutStrLn stderr ("No tldr entry for " <> unwords pages)
              exitFailure
            else handleTldrOpts
                   (opts
                      { tldrAction =
                          ViewPage (englishViewOptions voptions) pages
                      })

updateNecessary :: TldrOpts -> IO Bool
updateNecessary TldrOpts{..} = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  dataDirExists <- doesDirectoryExist dataDir
  if not dataDirExists
    then return True
    else do
      lastCachedTime <- getModificationTime dataDir
      currentTime <- getCurrentTime
      let diffExceedsLimit limit
            = currentTime `diffUTCTime` lastCachedTime
              > fromIntegral limit * nominalDay
      return $ maybe False diffExceedsLimit autoUpdateInterval

updateTldrPages :: IO ()
updateTldrPages = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  removePathForcibly dataDir
  createDirectory dataDir
  putStrLn $ "Downloading tldr pages to " ++ dataDir
  response <- httpLBS $ parseRequest_ pagesUrl
  let zipArchive = toArchive $ getResponseBody response
  extractFilesFromArchive [OptDestination dataDir] zipArchive

computeLocale :: Maybe String -> Locale
computeLocale lang = case map toLower <$> lang of
                       Nothing -> Missing
                       Just ('e':'n':_) -> English
                       Just (a:b:'_':_) -> Other [a,b]
                       Just (a:b:c:'_':_) -> Other [a,b,c]
                       Just other -> Unknown other

getPagePath :: Locale -> String -> [String] -> IO (Maybe FilePath)
getPagePath locale page pDirs = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  let currentLocale = case locale of
                        English -> "pages"
                        Other xs -> "pages." <> xs
                        Unknown xs -> "pages." <> xs
                        Missing -> "pages"
      pageDir = dataDir </> currentLocale
      paths = map (\x -> pageDir </> x </> page <.> "md") pDirs
  foldr1 (<|>) <$> mapM pageExists paths

pageExists :: FilePath -> IO (Maybe FilePath)
pageExists fname = do
  exists <- doesFileExist fname
  if exists
    then return $ Just fname
    else return Nothing


getCheckDirs :: ViewOptions -> [String]
getCheckDirs voptions =
  case platformOption voptions of
    Nothing -> checkDirs
    Just platform -> nubOrd $ ["common", platform] <> checkDirs

getNoColorEnv :: IO ColorSetting
getNoColorEnv = do
  noColorSet <- lookupEnv "NO_COLOR"
  return $ case noColorSet of
    Just _ -> NoColor
    Nothing -> UseColor

-- | Strip out duplicates
nubOrd :: Ord a => [a] -> [a]
nubOrd = loop mempty
  where
    loop _ [] = []
    loop !s (a:as)
      | a `Set.member` s = loop s as
      | otherwise = a : loop (Set.insert a s) as
