{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import GHC.IO.Handle.FD (stdout)
import Options.Applicative
import Paths_tldr (version)
import System.Directory
import System.Environment (getArgs)
import System.FilePath
import System.Process.Typed
import Tldr

data TldrOpts =
  TldrOpts
    { tldrAction :: TldrCommand
    }
  deriving (Show)

data TldrCommand
  = UpdateIndex
  | ViewPage ViewOptions [String]
  deriving (Show, Eq, Ord)

data ViewOptions =
  ViewOptions
    { platformOption :: Maybe String
    }
  deriving (Show, Eq, Ord)

programOptions :: Parser TldrOpts
programOptions = (TldrOpts <$> (updateIndexCommand <|> viewPageCommand))

updateIndexCommand :: Parser TldrCommand
updateIndexCommand = flag UpdateIndex UpdateIndex (long "update" <> short 'u')

viewOptionsParser :: Parser ViewOptions
viewOptionsParser = ViewOptions <$> platformFlag

viewPageCommand :: Parser TldrCommand
viewPageCommand =
  ViewPage <$> viewOptionsParser <*>
  some (strArgument (metavar "COMMAND" <> help "name of the command"))

platformFlag :: Parser (Maybe String)
platformFlag =
  optional (strOption (long "platform" <> short 'p' <> metavar "PLATFORM"))

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

tldrParserInfo :: ParserInfo TldrOpts
tldrParserInfo =
  info
    (helper <*> versionOption <*> programOptions)
    (fullDesc <> progDesc "tldr Client program" <>
     header "tldr - Simplified and community-driven man pages")
  where
    versionOption :: Parser (a -> a)
    versionOption =
      infoOption
        (showVersion version)
        (long "version" <> short 'v' <> help "Show version")

pageExists :: FilePath -> IO (Maybe FilePath)
pageExists fname = do
  exists <- doesFileExist fname
  if exists
    then return $ Just fname
    else return Nothing

getPagePath :: String -> [String] -> IO (Maybe FilePath)
getPagePath page platformDirs = do
  dataDir <- getXdgDirectory XdgData tldrDirName
  let pageDir = dataDir </> "tldr" </> "pages"
      paths = map (\x -> pageDir </> x </> page <.> "md") platformDirs
  foldr1 (<|>) <$> mapM pageExists paths

getCheckDirs :: ViewOptions -> [String]
getCheckDirs voptions =
  case platformOption voptions of
    Nothing -> checkDirs
    Just platform -> ["common", platform]

handleTldrOpts :: TldrOpts -> IO ()
handleTldrOpts TldrOpts {..} = do
  case tldrAction of
    UpdateIndex -> updateTldrPages
    ViewPage voptions pages -> do
      let npage = intercalate "-" pages
      fname <- getPagePath npage (getCheckDirs voptions)
      case fname of
        Just path -> renderPage path stdout
        Nothing -> putStrLn ("No tldr entry for " <> npage)

main :: IO ()
main = do
  args <- getArgs
  case execParserPure (prefs showHelpOnEmpty) tldrParserInfo args of
    failOpts@(Failure _) -> handleParseResult failOpts >> return ()
    Success opts -> handleTldrOpts opts
    compOpts@(CompletionInvoked _) -> handleParseResult compOpts >> return ()
