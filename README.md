# tldr

[![Build Status](https://dev.azure.com/psibi2000/tldr-hs/_apis/build/status/psibi.tldr-hs?branchName=master)](https://dev.azure.com/psibi2000/tldr-hs/_build/latest?definitionId=5?branchName=master)

[![Hackage](https://img.shields.io/hackage/v/tldr.svg)](https://hackage.haskell.org/package/tldr)
[![Stackage Nightly](http://stackage.org/package/tldr/badge/nightly)](http://stackage.org/nightly/package/tldr)
[![Stackage LTS](http://stackage.org/package/tldr/badge/lts)](http://stackage.org/lts/package/tldr)

Haskell client for tldr

## Installation

See Github releases: https://github.com/psibi/tldr-hs/releases

Executables are available for all the three major platforms: Linux, Windows and MacOS.

Or

1. [Install stack](https://docs.haskellstack.org/en/stable/README/#how-to-install)
2. `stack install tldr`

## Usage

``` shellsession
$ tldr --help
tldr - Simplified and community-driven man pages

Usage: tldr [-v|--version] ((-u|--update) | [-p|--platform PLATFORM]
            [-L|--language LOCALE] COMMAND | (-a|--about))
  tldr Client program

Available options:
  -h,--help                Show this help text
  -v,--version             Show version
  -u,--update              Update offline cache of tldr pages
  -p,--platform PLATFORM   Prioritize specfic platform while searching. Valid
                           values include linux, osx, windows, sunos
  -L,--language LOCALE     Preferred language for the page returned
  COMMAND                  name of the command
  -a,--about               About this program
```

Or a much better example of the usage:

``` shellsession
$ tldr tldr
tldr
Simplified man pages.More information: https://tldr.sh.

 - Get typical usages of a command (hint: this is how you got here!):
   tldr {{command}}

 - Show the tar tldr page for linux:
   tldr -p {{linux}} {{tar}}

 - Get help for a git subcommand:
   tldr {{git checkout}}
```

## Snapshot

![tldr](https://cloud.githubusercontent.com/assets/737477/24076451/2a5a604c-0c57-11e7-9bf7-13d76e8e7f12.png)
