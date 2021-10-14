{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}

module Tldr.Parser where

import Prelude              hiding (takeWhile)
import Control.Applicative
import Data.Attoparsec.Combinator
import Data.Attoparsec.Text
import Data.Text                   (Text)

import qualified Data.Text as T

-- $setup
-- >>> :set -XOverloadedStrings
-- >>> import Data.Attoparsec.Text


-- | Parses '{{foo}}' blocks in CommonMark Code, such that:
--
-- * `ls {{foo}} bar` -> `[Left "ls ", Right "foo", Left " bar"]`
--
-- >>> parseOnly codeParser ""
-- Right []
-- >>> parseOnly codeParser "tar"
-- Right [Left "tar"]
-- >>> parseOnly codeParser "tar{"
-- Right [Left "tar{"]
-- >>> parseOnly codeParser "tar{{"
-- Right [Left "tar{{"]
-- >>> parseOnly codeParser "tar{{{"
-- Right [Left "tar{{{"]
-- >>> parseOnly codeParser "tar}"
-- Right [Left "tar}"]
-- >>> parseOnly codeParser "tar{{{b}"
-- Right [Left "tar{{{b}"]
-- >>> parseOnly codeParser "tar{{{b}}"
-- Right [Left "tar",Right "{b"]
-- >>> parseOnly codeParser "tar{{b}}}"
-- Right [Left "tar",Right "b}"]
-- >>> parseOnly codeParser "tar xf {{source.tar[.gz|.bz2|.xz]}} --directory={{directory}}"
-- Right [Left "tar xf ",Right "source.tar[.gz|.bz2|.xz]",Left " --directory=",Right "directory"]
codeParser :: Parser [Either Text Text]
codeParser = collectEither <$> outer
 where
  inner :: Parser [Either Text Text]
  inner = do
    _ <- char '{'
    _ <- char '{'
    l <- takeWhile (/= '}')
    e <- optional findEnd
    case e of
      Just e' -> (\o -> [Right (l <> e')         ] <> o) <$> (outer <|> pure [])
      Nothing -> (\o -> [Left  (T.pack "{{" <> l)] <> o) <$> (outer <|> pure [])
   where
    findEnd :: Parser Text
    findEnd = do
      c1 <- anyChar
      (p2, p3) <- peek2Chars
      case (c1, p2, p3) of
        ('}', Just '}', Just '}') -> (T.singleton '}' <>) <$> findEnd
        ('}', Just '}', _)        -> mempty <$ anyChar
        _                         -> fail ("Couldn't find end: " <> show (c1, p2, p3))

  outer :: Parser [Either Text Text]
  outer = do
    o  <- takeWhile (/= '{')
    (p1, p2) <- peek2Chars
    case (p1, p2) of
      (Just '{', Just '{') -> (\i   -> [Left o                   ] <> i) <$> (inner <|> ((\t -> [Left t]) <$> takeText))
      (Just '{', _)        -> (\a b -> [Left (o <> T.singleton a)] <> b) <$> anyChar <*> outer
      _                    -> pure [Left o]


-- | Collect both Lefts and Rights, mappending them to zore or one item per connected sublist.
--
-- >>> collectEither []
-- []
-- >>> collectEither [Right "abc", Right "def", Left "x", Left "z", Right "end"]
-- [Right "abcdef",Left "xz",Right "end"]
-- >>> collectEither [Right "", Right "def", Left "x", Left "", Right ""]
-- [Right "def",Left "x"]
collectEither :: (Eq a, Eq b, Monoid a, Monoid b) => [Either a b] -> [Either a b]
collectEither = go Nothing
 where
  go Nothing  [] = []
  go (Just !x) []
    | x == Right mempty || x == Left mempty = []
    | otherwise                             = [x]
  go Nothing           (Left  b:br) = go (Just (Left  b))        br
  go Nothing           (Right b:br) = go (Just (Right b))        br
  go (Just (Left !a))  (Left  b:br) = go (Just (Left (a <> b)))  br
  go (Just (Right !a)) (Right b:br) = go (Just (Right (a <> b))) br
  go (Just !a) xs
    | a == Right mempty || a == Left mempty = go Nothing xs
    | otherwise                             = a:go Nothing xs


-- | Peek 2 characters, not consuming any input.
peek2Chars :: Parser (Maybe Char, Maybe Char)
peek2Chars = lookAhead ((,) <$> optional anyChar <*> optional anyChar)
