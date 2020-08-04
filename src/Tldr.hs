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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.IO.Handle (Handle)
import System.Console.ANSI
import Tldr.Types (ConsoleSetting(..))

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

toSGR :: ConsoleSetting -> [SGR]
toSGR cons =
  [ SetItalicized (italic cons)
  , SetConsoleIntensity (consoleIntensity cons)
  , SetUnderlining (underline cons)
  , SetBlinkSpeed (blink cons)
  , SetColor Foreground (fgIntensity cons) (fgColor cons)
  ]

renderNode :: NodeType -> Handle -> IO ()
renderNode (TEXT txt) handle = TIO.hPutStrLn handle txt
renderNode (HTML_BLOCK txt) handle = TIO.hPutStrLn handle txt
renderNode (CODE_BLOCK _ txt) handle = TIO.hPutStrLn handle txt
renderNode (HTML_INLINE txt) handle = TIO.hPutStrLn handle txt
renderNode (CODE txt) handle = TIO.hPutStrLn handle ("   " <> txt)
renderNode LINEBREAK handle = TIO.hPutStrLn handle ""
renderNode (LIST _) handle = TIO.hPutStrLn handle "" >> TIO.hPutStr handle " - "
renderNode _ _ = return ()

changeConsoleSetting :: NodeType -> IO ()
changeConsoleSetting (HEADING _) = setSGR $ toSGR headingSetting
changeConsoleSetting BLOCK_QUOTE = setSGR $ toSGR headingSetting
changeConsoleSetting ITEM = setSGR $ toSGR $ defConsoleSetting {fgColor = Green}
changeConsoleSetting (CODE _) =
  setSGR $ toSGR $ defConsoleSetting {fgColor = Yellow}
changeConsoleSetting _ = return ()

handleSubsetNodeType :: NodeType -> Text
handleSubsetNodeType (HTML_BLOCK txt) = txt
handleSubsetNodeType (CODE_BLOCK _ txt) = txt
handleSubsetNodeType (TEXT txt) = txt
handleSubsetNodeType (HTML_INLINE txt) = txt
handleSubsetNodeType (CODE txt) = txt
handleSubsetNodeType _ = mempty

handleSubsetNode :: Node -> Text
handleSubsetNode (Node _ ntype xs) =
  handleSubsetNodeType ntype <> T.concat (Prelude.map handleSubsetNode xs)

handleParagraph :: [Node] -> Handle -> IO ()
handleParagraph xs handle =
  TIO.hPutStrLn handle $ T.concat $ Prelude.map handleSubsetNode xs

handleNode :: Node -> Handle -> IO ()
handleNode (Node _ PARAGRAPH xs) handle = handleParagraph xs handle
handleNode (Node _ ITEM xs) handle =
  changeConsoleSetting ITEM >> handleParagraph xs handle
handleNode (Node _ ntype xs) handle = do
  changeConsoleSetting ntype
  renderNode ntype handle
  mapM_
    (\(Node _ ntype' ns) ->
       renderNode ntype' handle >> mapM_ (`handleNode` handle) ns)
    xs
  setSGR [Reset]

parsePage :: FilePath -> IO Node
parsePage fname = do
  page <- TIO.readFile fname
  let node = commonmarkToNode [] page
  return node

renderPage :: FilePath -> Handle -> IO ()
renderPage fname handle = do
  node <- parsePage fname
  handleNode node handle
