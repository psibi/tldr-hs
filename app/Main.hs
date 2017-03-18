module Main where

import Tldr
import Options.Applicative
import Data.Semigroup ((<>))
import Control.Monad
import System.Directory
import System.FilePath
import Data.Conduit.Shell hiding (info)

data TldrOpts = TldrOpts
  { pageName :: String
  , updateFlag :: Bool
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
  let repoDir = homeDir </> tldrDirName <> "tldr"
  run $
    do cd repoDir
       git "pull" ["origin", "master"]

tldrParserInfo :: ParserInfo TldrOpts
tldrParserInfo =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "tldr Client program" <>
     header "tldr - Simplified and community-driven man pages")
  where
    versionOption = infoOption "0.1" (long "version" <> help "Show version")
    programOptions :: Parser TldrOpts
    programOptions =
      TldrOpts <$>
      (strArgument (metavar "COMMAND" <> help "name of the command")) <*>
      (switch (long "update" <> help "Update tldr pages"))

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
  opts <- execParser tldrParserInfo
  when (updateFlag opts) updateTldrPages
  fname <- getPagePath (pageName opts)
  case fname of
    Nothing -> putStrLn ("No tldr entry for " <> (pageName opts))
    Just fpath -> renderPage fpath
