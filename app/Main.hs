{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Monad
import Data.List (intercalate, isPrefixOf)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import GHC.IO.Handle.FD (stdout)
import Options.Applicative hiding ((<>))
import Paths_tldr (version)
import System.Directory
import System.Environment (getArgs, withArgs)
import System.FilePath
import System.Process.Typed
import Tldr

data TldrOpts = TldrOpts
  { pageName :: String
  } deriving (Show)

tldrDirName :: String
tldrDirName = "tldr"

repoHttpsUrl :: String
repoHttpsUrl = "https://github.com/tldr-pages/tldr.git"

checkDirs :: [String]
checkDirs = ["common", "linux", "osx", "windows", "sunos"]

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

updateOption :: Parser (a -> a)
updateOption = infoOption "update" (long "update" <> help "Update tldr pages")

tldrParserInfo :: ParserInfo TldrOpts
tldrParserInfo =
  info
    (helper <*> versionOption <*> updateOption <*> programOptions)
    (fullDesc <> progDesc "tldr Client program" <>
     header "tldr - Simplified and community-driven man pages")
  where
    versionOption :: Parser (a -> a)
    versionOption =
      infoOption
        (showVersion version)
        (long "version" <> short 'v' <> help "Show version")

programOptions :: Parser TldrOpts
programOptions =
  (TldrOpts <$> strArgument (metavar "COMMAND" <> help "name of the command"))

pageExists :: FilePath -> IO (Maybe FilePath)
pageExists fname = do
  exists <- doesFileExist fname
  if exists
    then return $ Just fname
    else return Nothing

getPagePath :: String -> IO (Maybe FilePath)
getPagePath page = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  let pageDir = dataDir </> "tldr" </> "pages"
      paths = map (\x -> pageDir </> x </> page <.> "md") checkDirs
  foldr1 (<|>) <$> mapM pageExists paths

isOption :: String -> Bool
isOption string = string `isPrefixOf` "--"

hasOption :: [String] -> Bool
hasOption xs = any isOption xs

main :: IO ()
main = do
  args <- getArgs
  case execParserPure (prefs showHelpOnEmpty) tldrParserInfo args of
    failOpts@(Failure _)
      | args == ["--update"] -> updateTldrPages
      | otherwise ->
        if hasOption args
          then handleParseResult failOpts >> return ()
          else do
            let npage = intercalate "-" args
            fname <- getPagePath npage
            case fname of
              Just path -> renderPage path stdout
              Nothing ->
                putStrLn ("No tldr entry for " <> (intercalate " " args))
    Success opts -> do
      initializeTldrPages
      let page = pageName opts
      fname <- getPagePath page
      maybe
        (putStrLn ("No tldr entry for " <> page))
        (flip renderPage stdout)
        fname
    compOpts@(CompletionInvoked _) -> handleParseResult compOpts >> return ()
