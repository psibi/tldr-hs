module Tldr.App.Constant where

tldrDirName :: String
tldrDirName = "tldr"

pagesUrl :: String
pagesUrl = "https://tldr.sh/assets/tldr.zip"

checkDirs :: [String]
checkDirs = "common" : platformDirs

platformDirs :: [String]
platformDirs = ["linux", "osx", "windows", "sunos"]
