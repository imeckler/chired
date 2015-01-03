{-# LANGUAGE LambdaCase, OverloadedStrings #-}
module PostContentChunk

( PostContentChunk(..)
, PostContent(..)
) where

import Text.Parsec
import Web.Scotty
import qualified Data.Text.Lazy as TL
import Control.Applicative hiding (many, (<|>))

newtype PostContent = PostContent [PostContentChunk]
  deriving (Show, Read)

data PostContentChunk
  = Text TL.Text
  | Link String String
  deriving (Show, Read)

symbol s = string s <* spaces

postContentChunk = textChunk <|> link where
  textChunk = Text . TL.pack <$> many1 (noneOf "[")
  link = Link <$> between (symbol "[") (symbol "]") (many (noneOf "]"))
              <*> between (symbol "(") (symbol ")") (many (noneOf ")"))

postContent = PostContent <$> many postContentChunk

mapLeft f x = case x of { Left e -> Left (f e); Right r -> Right r }

instance Parsable PostContent where
  parseParam = mapLeft (TL.pack . show) . parse postContent ""

