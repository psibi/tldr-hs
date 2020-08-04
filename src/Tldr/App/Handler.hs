{-#LANGUAGE RecordWildCards#-}
{-# LANGUAGE BangPatterns #-}

module Tldr.App.Handler where

import Data.Version (showVersion)
import qualified Data.Set as Set
import Paths_tldr (version)
import System.Environment (getExecutablePath)
import System.Directory (doesFileExist, XdgDirectory(..), getXdgDirectory, createDirectoryIfMissing, doesDirectoryExist)
import System.FilePath ((</>), (<.>))
import System.Process.Typed
import Data.Semigroup ((<>))
import Options.Applicative
import Data.List (intercalate)
import System.Environment (getArgs, lookupEnv)
import Tldr
import Tldr.Types
import Tldr.App.Constant
import Control.Monad (unless)
import Data.Char (toLower)
import System.Exit (exitFailure)
import System.IO (stdout, stderr, hPutStrLn)

handleAboutFlag :: IO ()
handleAboutFlag = do
  path <- getExecutablePath
  let content =
        unlines
          [ path <> " v" <> (showVersion version)
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
handleTldrOpts opts@TldrOpts {..} = do
  case tldrAction of
    UpdateIndex -> updateTldrPages
    About -> handleAboutFlag
    vopts@(ViewPage voptions pages) -> do
      let npage = intercalate "-" pages
      locale <-
        case (languageOption voptions) of
          Nothing -> retriveLocale
          Just lg -> pure $ computeLocale (Just lg)
      fname <- getPagePath locale npage (getCheckDirs voptions)
      case fname of
        Just path -> renderPage path stdout
        Nothing -> do
          if checkLocale locale
            then do
              hPutStrLn stderr ("No tldr entry for " <> (intercalate " " pages))
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
  case repoExists of
    True ->
      runProcess_ $
      setWorkingDir (repoDir) $ proc "git" ["pull", "origin", "master"]
    False -> initializeTldrPages

computeLocale :: Maybe String -> Locale
computeLocale lang = case map toLower <$> lang of
                       Nothing -> Missing
                       Just ('e':'n':_) -> English
                       Just (a:b:'_':_) -> Other (a:b:[])
                       Just (a:b:c:'_':_) -> Other (a:b:c:[])
                       Just str -> Unknown str

getPagePath :: Locale -> String -> [String] -> IO (Maybe FilePath)
getPagePath locale page platformDirs = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  let currentLocale = case locale of
                        English -> "pages"
                        Other xs -> "pages." <> xs
                        Unknown xs -> "pages." <> xs
                        Missing -> "pages"
      pageDir = dataDir </> "tldr" </> currentLocale
      paths = map (\x -> pageDir </> x </> page <.> "md") platformDirs
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
