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
import System.IO (stdout, stderr, hPutStrLn)
import Options.Applicative
import Paths_tldr (version)
import System.Directory
import System.Environment (getArgs, getExecutablePath, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath
import System.Process.Typed
import Data.Char (toLower)
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

data ViewOptions =
  ViewOptions
    { platformOption :: Maybe String
    , languageOption :: Maybe String
    }
  deriving (Show, Eq, Ord)

englishViewOptions :: ViewOptions -> ViewOptions
englishViewOptions xs = xs { languageOption = Just "en_US.utf8" }

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
viewOptionsParser = ViewOptions <$> platformFlag <*> languageFlag

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

languageFlag :: Parser (Maybe String)
languageFlag =
  optional
    (strOption
       (long "language" <> short 'L' <> metavar "LOCALE" <>
        help
          ("Preferred language for the page returned")))

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

checkLocale :: Locale -> Bool
checkLocale English = True
checkLocale _ = False

data Locale = English | Missing | Other String | Unknown String

retriveLocale :: IO Locale
retriveLocale = do
  lang <- lookupEnv "LANG"
  pure $ computeLocale lang
          
computeLocale :: Maybe String -> Locale
computeLocale lang = case map toLower <$> lang of
                       Nothing -> Missing
                       Just ('e':'n':_) -> English
                       Just (a:b:'_':_) -> Other (a:b:[])
                       Just (a:b:c:'_':_) -> Other (a:b:c:[])
                       Just str -> Unknown str

main :: IO ()
main = do
  args <- getArgs
  case execParserPure (prefs showHelpOnEmpty) tldrParserInfo args of
    failOpts@(Failure _) -> handleParseResult failOpts >> return ()
    Success opts -> handleTldrOpts opts
    compOpts@(CompletionInvoked _) -> handleParseResult compOpts >> return ()
