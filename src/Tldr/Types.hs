module Tldr.Types where

import System.Console.ANSI

data Locale = English | Missing | Other String | Unknown String

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
