{-# LANGUAGE RecordWildCards,
             OverloadedStrings,
             NamedFieldPuns,
             ViewPatterns #-}
module Main where

import Text.Read (readMaybe)
import LDAP
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Web.Scotty
import Web.ClientSession
import qualified Network.Wai as W
import Network.Wai.Middleware.Static

data Credentials = Credentials { username :: String, password :: String}
type OrError = Either String

data Vote = Down | None | Up
  deriving (Show, Read, Eq)

voteToInt v = case v of { Up -> 1; None -> 0; Down -> -1 }

type ID = Int
type Username = TL.Text
data Post = Post
  { poster  :: Username
  , votes   :: M.Map Username Vote
  , idNum   :: ID
  , title   :: TL.Text
  , content :: TL.Text
  }
  deriving (Show, Read)

type PostDB = M.Map ID Post
data AppState = AppState
  { postDB :: PostDB
  , uid    :: Int
  }
  deriving (Show, Read)

-- Set cookie

authorize :: String -> String -> IO (Maybe ())
authorize cnetId password = handleLDAP (\err -> print err >> return Nothing) $ do
  c <- LDAP.ldapInitialize "ldaps://ldap.uchicago.edu:636"
  ldapSimpleBind c ("uid=" ++ cnetId ++ ",ou=people,dc=uchicago,dc=edu") password
  return (Just ())

bracket l r x = l <> r <> x

postsForUser :: AppState -> Username -> TL.Text
postsForUser (AppState{..}) u = bracket "[" "]" . TL.intercalate "," $ map postToJSON (M.elems postDB) where
  postToJSON (Post{..}) = bracket "{" "}" $
    TL.intercalate "," [kv "vote" (show vote), kv "title" (show title), kv "score" (show score)]
    where
    vote   = fromMaybe None (M.lookup u votes)
    score  = sum . map voteToInt $ M.elems votes
    kv k v = TL.pack (show k) <> ":" <> TL.pack v

getAppState :: IO AppState
getAppState = (fromMaybe (error "hi") . readMaybe) <$> readFile "posts"

postsToJSON :: PostDB -> TL.Text
postsToJSON posts = bracket "[" "]" $ TL.intercalate "," (map postToJSON (M.toList posts)) where
  postToJSON (t, s) = bracket "{" "}" $
    "\"title\":" <> TL.pack (show t) <> ",\"score\":" <> TL.pack (show s)

newPost appState poster title content = do
  AppState {..} <- readMVar appState
  let post = Post {poster, title, content, votes = M.empty, idNum = uid}
  putMVar appState (AppState {uid = uid + 1, postDB = M.insert uid post postDB})
  print =<< readMVar appState

errorPage = raise "Bad cookie"

loggedInUser key = do
  cookieMay <- header "cookie"
  return $ do
    c <- cookieMay
    fmap (TL.fromStrict . T.decodeUtf8) $
      decrypt key (T.encodeUtf8 $ TL.toStrict c)

loggedInUserErr key = maybe errorPage return =<< loggedInUser key

data UserAction = ClickUp | ClickDown

data Update
  = SetVote ID Vote
  | SetDB (M.Map ID Post)

instance ToJSON Vote where
  toJSON = toJSON . show

instance ToJSON Update where
  toJSON (SetVote idNum v) = object ["idNum" .= idNum, "vote" .=  v]

main :: IO ()
main = do
  appState <- newMVar =<< getAppState
  key <- getDefaultKey

  let vote dir = do
        postId <- param "postId"
        user   <- loggedInUserErr key
        v'     <- liftIO . modifyMVar appState $ \s@(AppState{..}) ->
          let v  = fromMaybe None $ M.lookup user . votes =<< M.lookup postId postDB
              v' = updateVote dir v
              db' = M.alter (fmap (\p -> p { votes = M.insert user v' (votes p)})) postId postDB
          in
          return (s {postDB = db'}, v')
        text . TL.decodeUtf8 . Data.Aeson.encode $ SetVote postId v'

  scotty 3000 $ do
    middleware $ staticPolicy (noDots <> addBase "static")

    post "/login" $ do
      cnetId   <- param "username"
      password <- param "password"
      liftIO (authorize cnetId password) >>= \case
        Nothing -> raise "password no work"
        Just () -> do
          c <- encryptIO key cnetId
          setHeader "Set-Cookie" c

    get "/" $ do
      setHeader "Content-Type" "text/html"
      file "static/index.html"

    get "/posts" $
      text . postsToJSON . postDB =<< liftIO (readMVar appState)

    get "/newpost" $ do
      title     <- param "title"
      content   <- param "content"
      cookieMay <- header "cookie"
      poster    <- loggedInUserErr key
      liftIO $ newPost appState poster title content
      redirect "/"

    get "/actions/noop" $ return ()

    get "/actions/up/:postId" $ vote ClickUp

    get "/actions/down/:postId" $ vote ClickDown

  where
  updateVote :: UserAction -> Vote -> Vote
  updateVote ClickUp   Up   = None
  updateVote ClickDown Down = None
  updateVote ClickUp   _    = Up
  updateVote ClickDown _    = Down

