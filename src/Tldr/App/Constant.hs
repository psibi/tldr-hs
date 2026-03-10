module Tldr.App.Constant where

tldrDirName :: String
tldrDirName = "tldr"

pagesUrl :: String
pagesUrl = "https://github.com/tldr-pages/tldr/releases/latest/download/tldr.zip"

checkDirs :: [String]
checkDirs = "common" : platformDirs

platformDirs :: [String]
platformDirs = ["linux", "osx", "windows", "sunos"]
