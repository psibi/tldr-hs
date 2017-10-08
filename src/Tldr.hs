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
import Data.List ((++))
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

renderNode :: NodeType -> (Maybe NodeType) -> IO ()
renderNode (TEXT txt) ntype = case ntype of
                                Just (HEADING _) -> TIO.putStrLn txt
                                Just (BLOCK_QUOTE) -> TIO.putStrLn txt
                                Just tr -> changeConsoleSetting ITEM >> TIO.putStrLn (" - " <> txt)
                                Nothing -> TIO.putStrLn txt
renderNode (HTML_BLOCK txt) _ = TIO.putStrLn txt
renderNode (CODE_BLOCK _ txt) _ = changeConsoleSetting (CODE undefined) >> TIO.putStrLn ("   " <> txt) >> changeConsoleSetting ITEM
renderNode LINEBREAK _ = TIO.putStrLn ""
renderNode (LIST _) _ = TIO.putStrLn "" >> TIO.putStr " - "
renderNode _ _ = return ()

changeConsoleSetting :: NodeType -> IO ()
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

handleNode :: Node -> Maybe NodeType -> IO ()
handleNode (Node _ ntype xs) mntype = do
  renderNode ntype mntype
  mapM_ (\(Node _ ntype' ns) -> renderNode ntype' mntype >> mapM_ (\x -> handleNode x (Just ntype')) ns) xs

parsePage :: FilePath -> IO Node
parsePage fname = do
  page <- TIO.readFile fname
  let node = commonmarkToNode [] page
  return node

-- This is for debugging
printNode :: Node -> IO ()
printNode (Node _ ntype ns) = do
  putStrLn $ "Node type: " <> (show ntype)
  mapM_ printNode ns

renderPage :: FilePath -> IO ()
renderPage fname = do
  node <- parsePage fname
  handleNode node Nothing
