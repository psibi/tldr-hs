module Tldr.App.Constant where

tldrDirName :: String
tldrDirName = "tldr"

repoHttpsUrl :: String
repoHttpsUrl = "https://github.com/tldr-pages/tldr.git"

checkDirs :: [String]
checkDirs = "common" : platformDirs

platformDirs :: [String]
platformDirs = ["linux", "osx", "windows", "sunos"]
