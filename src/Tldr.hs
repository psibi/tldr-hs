{-# LANGUAGE OverloadedStrings #-}

module Tldr
  ( parsePage
  , renderPage
  , ConsoleSetting(..)
  , defConsoleSetting
  , headingSetting
  , toSGR
  , renderNode
  , changeConsoleSetting
  ) where

import Data.Text
import qualified Data.Text.IO as TIO
import CMark
import System.Console.ANSI
import Data.Monoid ((<>))

data ConsoleSetting = ConsoleSetting
  { italic :: Bool
  , underline :: Underlining
  , blink :: BlinkSpeed
  , fgIntensity :: ColorIntensity
  , fgColor :: Color
  , bgIntensity :: ColorIntensity
  , consoleIntensity :: ConsoleIntensity
  }

defConsoleSetting :: ConsoleSetting
defConsoleSetting =
  ConsoleSetting
  { italic = False
  , underline = NoUnderline
  , blink = NoBlink
  , fgIntensity = Dull
  , fgColor = White
  , bgIntensity = Dull
  , consoleIntensity = NormalIntensity
  }

headingSetting :: ConsoleSetting
headingSetting =
  defConsoleSetting
  { consoleIntensity = BoldIntensity
  }

toSGR :: ConsoleSetting -> [SGR]
toSGR cons =
  [ SetItalicized (italic cons)
  , SetConsoleIntensity (consoleIntensity cons)
  , SetUnderlining (underline cons)
  , SetBlinkSpeed (blink cons)
  , SetColor Foreground (fgIntensity cons) (fgColor cons)
  ]

renderNode :: NodeType -> IO ()
renderNode (TEXT txt) = TIO.putStrLn txt
renderNode (HTML_BLOCK txt) = TIO.putStrLn txt
renderNode (CODE_BLOCK _ txt) = TIO.putStrLn txt
renderNode (HTML_INLINE txt) = TIO.putStrLn txt
renderNode (CODE txt) = TIO.putStrLn ("   " <> txt)
renderNode LINEBREAK = TIO.putStrLn ""
renderNode (LIST _) = TIO.putStrLn "" >> TIO.putStr " - "
renderNode _ = return ()

changeConsoleSetting :: NodeType -> IO ()
changeConsoleSetting (HEADING _) = setSGR $ toSGR headingSetting
changeConsoleSetting BLOCK_QUOTE = setSGR $ toSGR headingSetting
changeConsoleSetting ITEM =
  setSGR $
  toSGR $
  defConsoleSetting
  { fgColor = Green
  }
changeConsoleSetting (CODE _) =
  setSGR $
  toSGR $
  defConsoleSetting
  { fgColor = Yellow
  }
changeConsoleSetting _ = return ()

handleNode :: Node -> IO ()
handleNode (Node _ ntype xs) = do
  changeConsoleSetting ntype
  renderNode ntype
  mapM_ (\(Node _ ntype' ns) -> renderNode ntype' >> mapM_ handleNode ns) xs
  setSGR [Reset]

parsePage :: FilePath -> IO Node
parsePage fname = do
  page <- TIO.readFile fname
  let node = commonmarkToNode [] page
  return node

renderPage :: FilePath -> IO ()
renderPage fname = do
  node <- parsePage fname
  handleNode node
