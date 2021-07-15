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

import CMark
import Data.Monoid ((<>))
import Data.Text hiding (cons)
import GHC.IO.Handle (Handle)
import System.Console.ANSI
import Tldr.Types (ConsoleSetting(..), ColorSetting (..))
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

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
headingSetting = defConsoleSetting {consoleIntensity = BoldIntensity}

toSGR :: ColorSetting -> ConsoleSetting -> [SGR]
toSGR color cons = case color of
  NoColor -> def
  UseColor -> SetColor Foreground (fgIntensity cons) (fgColor cons) : def
  where
    def =
      [ SetItalicized (italic cons)
      , SetConsoleIntensity (consoleIntensity cons)
      , SetUnderlining (underline cons)
      , SetBlinkSpeed (blink cons)
      ]

renderNode :: NodeType -> Handle -> IO ()
renderNode (TEXT txt) handle = TIO.hPutStrLn handle (txt <> "\n")
renderNode (HTML_BLOCK txt) handle = TIO.hPutStrLn handle txt
renderNode (CODE_BLOCK _ txt) handle = TIO.hPutStrLn handle txt
renderNode (HTML_INLINE txt) handle = TIO.hPutStrLn handle txt
renderNode (CODE txt) handle = TIO.hPutStrLn handle ("   " <> txt)
renderNode LINEBREAK handle = TIO.hPutStrLn handle ""
renderNode (LIST _) handle = TIO.hPutStrLn handle "" >> TIO.hPutStr handle " - "
renderNode _ _ = return ()

changeConsoleSetting :: ColorSetting -> NodeType -> IO ()
changeConsoleSetting color (HEADING _) = setSGR $ toSGR color headingSetting
changeConsoleSetting color BLOCK_QUOTE = setSGR $ toSGR color headingSetting
changeConsoleSetting color ITEM = setSGR $ toSGR color $ defConsoleSetting {fgColor = Green}
changeConsoleSetting color (CODE _) =
  setSGR $ toSGR color $ defConsoleSetting {fgColor = Yellow}
changeConsoleSetting _ _ = return ()

handleSubsetNodeType :: NodeType -> Text
handleSubsetNodeType (HTML_BLOCK txt) = txt
handleSubsetNodeType (CODE_BLOCK _ txt) = txt
handleSubsetNodeType (TEXT txt) = txt
handleSubsetNodeType (HTML_INLINE txt) = txt
handleSubsetNodeType (CODE txt) = txt
handleSubsetNodeType _ = mempty

handleSubsetNode :: Node -> Text
handleSubsetNode (Node _ SOFTBREAK _) = "\n"
handleSubsetNode (Node _ ntype xs) =
  handleSubsetNodeType ntype <> T.concat (Prelude.map handleSubsetNode xs)

handleParagraph :: [Node] -> Handle -> IO ()
handleParagraph xs handle =
  TIO.hPutStrLn handle $ T.concat $ Prelude.map handleSubsetNode xs

handleNode :: Node -> Handle -> ColorSetting -> IO ()
handleNode (Node _ PARAGRAPH xs) handle _ = handleParagraph xs handle
handleNode (Node _ ITEM xs) handle color =
  changeConsoleSetting color ITEM >> handleParagraph xs handle
handleNode (Node _ ntype xs) handle color = do
  changeConsoleSetting color ntype
  renderNode ntype handle
  mapM_
    (\(Node _ ntype' ns) ->
       renderNode ntype' handle >> mapM_ (\n -> handleNode n handle color) ns)
    xs
  setSGR [Reset]

parsePage :: FilePath -> IO Node
parsePage fname = do
  page <- TIO.readFile fname
  let node = commonmarkToNode [] page
  return node

renderPage :: FilePath -> Handle -> ColorSetting -> IO ()
renderPage fname handle color = do
  node <- parsePage fname
  handleNode node handle color
