{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

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
import Control.Monad (forM_)
import Data.Attoparsec.Text
import Data.Monoid ((<>))
import Data.Text hiding (cons)
import GHC.IO.Handle (Handle)
import System.Console.ANSI
import Tldr.Parser
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

reset :: ColorSetting -> IO ()
reset color = case color of
  NoColor -> pure ()
  UseColor -> setSGR [Reset]

renderNode :: NodeType -> ColorSetting -> Handle -> IO ()
renderNode nt@(TEXT txt) color handle = changeConsoleSetting color nt >> TIO.hPutStrLn handle (txt <> "\n") >> reset color
renderNode nt@(HTML_BLOCK txt) color handle = changeConsoleSetting color nt >> TIO.hPutStrLn handle txt >> reset color
renderNode nt@(CODE_BLOCK _ txt) color handle = changeConsoleSetting color nt >> TIO.hPutStrLn handle txt >> reset color
renderNode nt@(HTML_INLINE txt) color handle = changeConsoleSetting color nt >> TIO.hPutStrLn handle txt >> reset color
renderNode (CODE txt) color handle = renderCode color txt handle
renderNode nt@LINEBREAK color handle = changeConsoleSetting color nt >> TIO.hPutStrLn handle "" >> reset color
renderNode nt@(LIST _) color handle = changeConsoleSetting color nt >> TIO.hPutStrLn handle "" >> TIO.hPutStr handle " - " >> reset color
renderNode _ _ _ = return ()

renderCode :: ColorSetting -> Text -> Handle -> IO ()
renderCode color txt handle = do
  TIO.hPutStr handle ("   ")
  case parseOnly codeParser txt of
    Right xs -> do
      forM_ xs $ \case
        Left x -> changeConsoleSetting color (CODE txt) >> TIO.hPutStr handle x >> reset color
        Right x -> TIO.hPutStr handle x
    Left _ -> changeConsoleSetting color (CODE txt) >> TIO.hPutStr handle txt >> reset color
  TIO.hPutStr handle ("\n")

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
  renderNode ntype color handle
  mapM_
    (\(Node _ ntype' ns) ->
       renderNode ntype' color handle >> mapM_ (\n -> handleNode n handle color) ns)
    xs
  reset color

parsePage :: FilePath -> IO Node
parsePage fname = do
  page <- TIO.readFile fname
  let node = commonmarkToNode [] page
  return node

renderPage :: FilePath -> Handle -> ColorSetting -> IO ()
renderPage fname handle color = do
  node <- parsePage fname
  handleNode node handle color
