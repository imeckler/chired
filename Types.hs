module Types
( PostContentChunk(..)
, PostContent(..)
, Vote(..)
, ID
, Username
, Post(..)
) where

import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import PostContentChunk

data Vote = Down | None | Up
  deriving (Show, Read, Eq)

type ID = Int
type Username = TL.Text

data Post = Post
  { poster  :: Username
  , votes   :: M.Map Username Vote
  , idNum   :: ID
  , title   :: TL.Text
  , content :: PostContent
  }
  deriving (Show, Read)


