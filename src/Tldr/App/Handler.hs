{-#LANGUAGE RecordWildCards#-}
{-# LANGUAGE BangPatterns #-}

module Tldr.App.Handler
  ( handleAboutFlag
  , retriveLocale
  , checkLocale
  , englishViewOptions
  , getCheckDirs
  , initializeTldrPages
  , pageExists
  , getPagePath
  , updateTldrPages
  , handleTldrOpts
  ) where

import Control.Monad (unless)
import Data.Char (toLower)
import Data.List (intercalate)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.Version (showVersion)
import Options.Applicative
import Paths_tldr (version)
import System.Directory
  ( XdgDirectory(..)
  , createDirectoryIfMissing
  , doesDirectoryExist
  , doesFileExist
  , getXdgDirectory
  )
import System.Environment (lookupEnv, getExecutablePath)
import System.Exit (exitFailure)
import System.FilePath ((<.>), (</>))
import System.IO (hPutStrLn, stderr, stdout)
import System.Process.Typed
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
      let npage = intercalate "-" pages
      locale <-
        case languageOption voptions of
          Nothing -> retriveLocale
          Just lg -> pure $ computeLocale (Just lg)
      fname <- getPagePath locale npage (getCheckDirs voptions)
      case fname of
        Just path -> renderPage path stdout
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

updateTldrPages :: IO ()
updateTldrPages = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  let repoDir = dataDir </> "tldr"
  repoExists <- doesDirectoryExist repoDir
  if repoExists
    then runProcess_ $
         setWorkingDir repoDir $ proc "git" ["pull", "origin", "master"]
    else initializeTldrPages

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
      pageDir = dataDir </> "tldr" </> currentLocale
      paths = map (\x -> pageDir </> x </> page <.> "md") pDirs
  foldr1 (<|>) <$> mapM pageExists paths

pageExists :: FilePath -> IO (Maybe FilePath)
pageExists fname = do
  exists <- doesFileExist fname
  if exists
    then return $ Just fname
    else return Nothing

tldrInitialized :: IO Bool
tldrInitialized = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  let dir2 = dataDir </> "tldr"
      pages = dataDir </> "tldr" </> "pages"
  exists <- mapM doesDirectoryExist [dataDir, dir2, pages]
  return $ all (== True) exists

initializeTldrPages :: IO ()
initializeTldrPages = do
  initialized <- tldrInitialized
  unless initialized $ do
    dataDir <- getXdgDirectory XdgData tldrDirName
    createDirectoryIfMissing False dataDir
    runProcess_ $ setWorkingDir dataDir $ proc "git" ["clone", repoHttpsUrl]

getCheckDirs :: ViewOptions -> [String]
getCheckDirs voptions =
  case platformOption voptions of
    Nothing -> checkDirs
    Just platform -> nubOrd $ ["common", platform] <> checkDirs


-- | Strip out duplicates
nubOrd :: Ord a => [a] -> [a]
nubOrd = loop mempty
  where
    loop _ [] = []
    loop !s (a:as)
      | a `Set.member` s = loop s as
      | otherwise = a : loop (Set.insert a s) as
