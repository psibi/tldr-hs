{-# LANGUAGE ScopedTypeVariables #-}

module Tldr.App
  ( appMain
  ) where

import Data.List (intercalate)
import Data.Semigroup ((<>))
import Data.Version (showVersion)
import Options.Applicative
import Paths_tldr (version)
import System.Environment (getArgs)
import Tldr.App.Constant (platformDirs)
import Tldr.App.Handler
import Tldr.Types
import Control.Monad (void)

programOptions :: Parser TldrOpts
programOptions =
  TldrOpts <$> (updateIndexCommand <|> viewPageCommand <|> aboutFlag) <*> autoUpdateIntervalOpt <*> colorFlags

updateIndexCommand :: Parser TldrCommand
updateIndexCommand =
  flag'
    UpdateIndex
    (long "update" <> short 'u' <> help "Update offline cache of tldr pages")

autoUpdateIntervalOpt :: Parser (Maybe Int)
autoUpdateIntervalOpt =
  optional
    (option auto
       (long "auto-update-interval" <> metavar "DAYS" <>
        help
          "Perform an automatic update if the cache is older than DAYS"))

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
          ("Prioritize a specific platform while searching. Valid values include " <>
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
          "Preferred language for the page returned"))

useColorFlag :: Parser (Maybe ColorSetting)
useColorFlag =
  optional
    (flag' UseColor
        (long "color" <>
        help
          "Force colored output, overriding the NO_COLOR environment variable"))

noColorFlag :: Parser (Maybe ColorSetting)
noColorFlag =
  optional
    (flag' NoColor
        (long "no-color" <>
        help
          "Disable colored output")) 

colorFlags :: Parser (Maybe ColorSetting)
colorFlags = useColorFlag <|> noColorFlag

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
    failOpts@(Failure _) -> void $ handleParseResult failOpts
    Success opts -> handleTldrOpts opts
    compOpts@(CompletionInvoked _) -> void $ handleParseResult compOpts
