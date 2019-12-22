{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

module Main
  ( main
  ) where

import Control.Monad
import Data.List (intercalate)
import Data.Semigroup ((<>))
import qualified Data.Set as Set
import Data.Version (showVersion)
import GHC.IO.Handle.FD (stdout)
import Options.Applicative
import Paths_tldr (version)
import System.Directory
import System.Environment (getArgs, getExecutablePath)
import System.FilePath
import System.Process.Typed
import Tldr

data TldrOpts = TldrOpts
  { tldrAction :: TldrCommand
  } deriving (Show)

data TldrCommand
  = UpdateIndex
  | ViewPage ViewOptions
             [String]
  | About
  deriving (Show, Eq, Ord)

data ViewOptions = ViewOptions
  { platformOption :: Maybe String
  } deriving (Show, Eq, Ord)

programOptions :: Parser TldrOpts
programOptions =
  (TldrOpts <$> (updateIndexCommand <|> viewPageCommand <|> aboutFlag))

updateIndexCommand :: Parser TldrCommand
updateIndexCommand =
  flag'
    UpdateIndex
    (long "update" <> short 'u' <> help "Update offline cache of tldr pages")

aboutFlag :: Parser TldrCommand
aboutFlag = flag' About (long "about" <> short 'a' <> help "About this program")

viewOptionsParser :: Parser ViewOptions
viewOptionsParser = ViewOptions <$> platformFlag

viewPageCommand :: Parser TldrCommand
viewPageCommand =
  ViewPage <$> viewOptionsParser <*>
  some (strArgument (metavar "COMMAND" <> help "name of the command"))

platformFlag :: Parser (Maybe String)
platformFlag =
  optional
    (strOption
       (long "platform" <> short 'p' <> metavar "PLATFORM" <>
        help
          ("Prioritize specfic platform while searching. Valid values include " <>
           platformHelpValue)))
  where
    platformHelpValue :: String
    platformHelpValue = intercalate ", " platformDirs

tldrDirName :: String
tldrDirName = "tldr"

repoHttpsUrl :: String
repoHttpsUrl = "https://github.com/tldr-pages/tldr.git"

checkDirs :: [String]
checkDirs = "common" : platformDirs

platformDirs :: [String]
platformDirs = ["linux", "osx", "windows", "sunos"]

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
    Just platform -> nubOrd $ ["common", platform] <> checkDirs

-- | Strip out duplicates
nubOrd :: Ord a => [a] -> [a]
nubOrd = loop mempty
  where
    loop _ [] = []
    loop !s (a:as)
      | a `Set.member` s = loop s as
      | otherwise = a : loop (Set.insert a s) as

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

handleTldrOpts :: TldrOpts -> IO ()
handleTldrOpts TldrOpts {..} = do
  case tldrAction of
    UpdateIndex -> updateTldrPages
    About -> handleAboutFlag
    ViewPage voptions pages -> do
      let npage = intercalate "-" pages
      fname <- getPagePath npage (getCheckDirs voptions)
      case fname of
        Just path -> renderPage path stdout
        Nothing -> putStrLn ("No tldr entry for " <> (intercalate " " pages))

main :: IO ()
main = do
  args <- getArgs
  case execParserPure (prefs showHelpOnEmpty) tldrParserInfo args of
    failOpts@(Failure _) -> handleParseResult failOpts >> return ()
    Success opts -> handleTldrOpts opts
    compOpts@(CompletionInvoked _) -> handleParseResult compOpts >> return ()
