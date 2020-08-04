{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}

module Tldr.App
  ( appMain
  ) where

import Control.Monad
import Data.List (intercalate)
import Data.Semigroup ((<>))
import Tldr.App.Constant
import Data.Version (showVersion)
import System.IO (stdout, stderr, hPutStrLn)
import Options.Applicative
import Paths_tldr (version)
import System.Directory
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath
import System.Process.Typed
import Data.Char (toLower)
import Tldr
import Tldr.App.Handler
import Tldr.Types


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


appMain :: IO ()
appMain = do
  args <- getArgs
  case execParserPure (prefs showHelpOnEmpty) tldrParserInfo args of
    failOpts@(Failure _) -> handleParseResult failOpts >> return ()
    Success opts -> handleTldrOpts opts
    compOpts@(CompletionInvoked _) -> handleParseResult compOpts >> return ()
