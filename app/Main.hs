module Main where

import Tldr
import Options.Applicative hiding ((<>))
import Data.Semigroup ((<>))
import Control.Monad
import System.Directory
import System.FilePath
import Data.Conduit.Shell hiding (info)
import System.Environment (getArgs, withArgs)

data TldrOpts = TldrOpts
  { pageName :: String
  } deriving (Show)

tldrDirName :: String
tldrDirName = ".tldr"

repoHttpsUrl :: String
repoHttpsUrl = "https://github.com/tldr-pages/tldr.git"

checkDirs :: [String]
checkDirs = ["common", "linux", "osx"]

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
  if initialized
    then return ()
    else do
      homeDir <- getHomeDirectory
      run $
        do mkdir (homeDir </> tldrDirName)
           cd (homeDir </> tldrDirName)
           git "clone" repoHttpsUrl

updateTldrPages :: IO ()
updateTldrPages = do
  homeDir <- getHomeDirectory
  let repoDir = homeDir </> tldrDirName </> "tldr"
  run $
    do cd repoDir
       git "pull" ["origin", "master"]

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
      infoOption "0.1" (long "version" <> short 'v' <> help "Show version")

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
      x@(f1:f2:f3:[]) = map (\x -> pageDir </> x </> page <.> "md") checkDirs
  pageExists f1 <|> pageExists f2 <|> pageExists f3

main :: IO ()
main = do
  initializeTldrPages
  args <- getArgs
  case execParserPure (prefs noBacktrack) tldrParserInfo args of
    Failure _
      | null args -> withArgs ["--help"] (execParser tldrParserInfo) >> return ()
      | args == ["--update"] -> updateTldrPages
    parseResult -> do
      opts <- handleParseResult parseResult
      let page = pageName opts
      fname <- getPagePath page
      maybe (putStrLn ("No tldr entry for " <> page)) renderPage fname
