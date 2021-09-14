module Tldr.Types where

import System.Console.ANSI

data Locale = English | Missing | Other String | Unknown String

data ColorSetting = NoColor | UseColor
  deriving (Eq, Show, Ord, Enum, Bounded)

data ConsoleSetting =
  ConsoleSetting
    { italic :: Bool
    , underline :: Underlining
    , blink :: BlinkSpeed
    , fgIntensity :: ColorIntensity
    , fgColor :: Color
    , bgIntensity :: ColorIntensity
    , consoleIntensity :: ConsoleIntensity
    }

data TldrOpts = TldrOpts
  { tldrAction :: TldrCommand
  , autoUpdateInterval :: Maybe Int
  , colorSetting :: Maybe ColorSetting
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
