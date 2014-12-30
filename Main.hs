{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}
module Main where

import LDAP
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import qualified Data.ByteString as B
import Data.Monoid
import Data.Maybe
import Web.Scotty
import Web.ClientSession
import qualified Network.Wai as W

data Credentials = Credentials { username :: String, password :: String}
type OrError = Either String

data Vote = Up | Down | None

type Title = String
type Username = String
type Post = Post
  { poster :: Username
  , votes  :: M.Map Username Vote
  , title  :: Title
  }
type PostDB = M.Map Title Int

authorize :: W.Request -> IO (OrError ())
authorize = maybe (return (Left "No Authorization header found")) auth . credentials where
  auth (Credentials {..}) = handleLDAP (\_ -> return (Left "")) $ do
    ld <- ldapInit "ldap.uchicago.edu" 636
    ldapSimpleBind ld ("uid=" ++ username ++ ",ou=people,dc=uchicago,dc=edu") password
    return (Right ())

  credentials :: W.Request -> Maybe Credentials
  credentials (W.requestHeaders -> h) =
    case filter ((== "Authorization") . fst) h of {[v] -> Just (decode v); _ -> Nothing}

  decode = undefined

bracket l r x = l <> r <> x

postsForUser :: PostDB -> Username -> T.Text
postsForUser posts u = bracket "[" "]" . T.intercalate "," $ map toJson (M.elems posts) where
  toJson (Post{..}) = bracket "{" "}" $
    T.intercalate "," [kv "vote" (show vote), kv "title" (show title), kv "score" (show score)]
    where
    vote = fromMaybe None (M.lookup u votes)
    kv k v = T.pack (show k) <> ":" <> T.pack v

readPostDB :: IO PostDB
readPostDB = read <$> readFile "posts"

postsToJSON :: PostDB -> T.Text
postsToJSON posts = bracket "[" "]" $ T.intercalate "," (map postToJSON (M.toList posts)) where
  postToJSON (t, s) = bracket "{" "}" $
    "\"title\":" <> T.pack (show t) <> ",\"score\":" <> T.pack (show s)

main :: IO ()
main = do
  postsVar <- newMVar =<< readPostDB
  key <- getDefaultKey

  scotty 3000 $ do
    get "/posts" $
      text . postsToJSON =<< liftIO (readMVar postsVar)

    get "*" $
      text "Hi!"

