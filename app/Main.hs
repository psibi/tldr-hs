{-#LANGUAGE ScopedTypeVariables#-}

module Main where

import Tldr
import Options.Applicative hiding ((<>))
import Data.Semigroup ((<>))
import Control.Monad
import System.Directory
import System.FilePath
import System.Process.Typed
import System.Environment (getArgs, withArgs)
import GHC.IO.Handle.FD (stdout)
import Paths_tldr (version)
import Data.Version (showVersion)

data TldrOpts = TldrOpts
  { pageName :: String
  } deriving (Show)

tldrDirName :: String
tldrDirName = ".tldr"

repoHttpsUrl :: String
repoHttpsUrl = "https://github.com/tldr-pages/tldr.git"

checkDirs :: [String]
checkDirs = ["common", "linux", "osx", "windows"]

tldrInitialized :: IO Bool
tldrInitialized = do
  homeDir <- getHomeDirectory
  let dir1 = homeDir </> tldrDirName
      dir2 = homeDir </> tldrDirName </> "tldr"
      pages = homeDir </> tldrDirName </> "tldr" </> "pages"
  exists <- mapM doesDirectoryExist [dir1, dir2, pages]
  return $ all (== True) exists

initializeTldrPages :: IO ()
initializeTldrPages = do
  initialized <- tldrInitialized
  unless initialized $ do
    homeDir <- getHomeDirectory
    let cloneDir = homeDir </> tldrDirName
    runProcess_ $ proc "mkdir" [cloneDir]
    runProcess_ $ setWorkingDir cloneDir $ proc "git" ["clone", repoHttpsUrl]


updateTldrPages :: IO ()
updateTldrPages = do
  homeDir <- getHomeDirectory
  let repoDir = homeDir </> tldrDirName </> "tldr"
  repoExists <- doesDirectoryExist repoDir
  when repoExists $ do
    runProcess_ $ setWorkingDir repoDir $ proc "git" ["pull", "origin", "master"] 

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
      infoOption (showVersion version) (long "version" <> short 'v' <> help "Show version")

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
  homeDir <- getHomeDirectory
  let pageDir = homeDir </> tldrDirName </> "tldr" </> "pages"
      paths = map (\x -> pageDir </> x </> page <.> "md") checkDirs
  foldr (<|>) Nothing <$> mapM pageExists paths

main :: IO ()
main = do
  args <- getArgs
  case execParserPure (prefs noBacktrack) tldrParserInfo args of
    failOpts@(Failure _)
      | args == ["--update"] -> updateTldrPages
      | otherwise -> handleParseResult failOpts >> return ()
    Success opts -> do
         initializeTldrPages
         let page = pageName opts
         fname <- getPagePath page
         maybe (putStrLn ("No tldr entry for " <> page)) (flip renderPage stdout) fname 
    compOpts@(CompletionInvoked _) -> handleParseResult compOpts >> return ()
