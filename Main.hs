{-# LANGUAGE RecordWildCards,
             OverloadedStrings,
             LambdaCase,
             NamedFieldPuns,
             ViewPatterns #-}
module Main where

import Control.Monad (void)
import Text.Read (readMaybe)
import LDAP
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.IO as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Encoding as T
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC8
import qualified Data.ByteString.Lazy as BL
import Data.Monoid
import Data.Aeson
import Data.Maybe
import Web.Scotty
import Web.ClientSession
import qualified Network.Wai as W
import Network.Wai.Middleware.Static
import System.Posix.Signals
import Types

type OrError = Either String

voteToInt v = case v of { Up -> 1; None -> 0; Down -> -1 }

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

bracket l r x = l <> x <> r

postsForUser :: Username -> AppState -> TL.Text
postsForUser u (AppState{..}) = jsonList postToJSON (M.elems postDB) where
  jsonList f = bracket "[" "]" . TL.intercalate "," . map f

  contentToJson (PostContent cs) = jsonList chunkToJson cs

  chunkToJson c = case c of
    Text t     -> TL.pack (show t)
    Link t url -> jsonObj [kv "text" (TL.pack $ show t), kv "url" (TL.pack $ show url)]

  jsonObj = bracket "{" "}" . TL.intercalate ","

  postToJSON (Post{..}) = jsonObj $
      [ kv "vote" vote
      , kv "title" (TL.pack $ show title)
      , kv "score" score
      , kv "idNum" (TL.pack $ show idNum)
      , kv "content" (contentToJson content)
      ]
    where
    vote   = TL.pack . show . show $ fromMaybe None (M.lookup u votes)
    score  = TL.pack . show . sum . map voteToInt $ M.elems votes

  kv k v = TL.pack (show k) <> ":" <> v

getAppState :: IO AppState
getAppState =
  (readMaybe <$> readFile "posts") >>= \case
    Nothing -> error "Bad serialized AppState" -- >> return defaultState
    Just x  -> return x
  where defaultState = AppState { postDB = M.empty, uid = 0 }

{-
postsToJSON :: PostDB -> TL.Text
postsToJSON posts = bracket "[" "]" $ TL.intercalate "," (map postToJSON (M.elems posts)) where
  postToJSON (t, s) = bracket "{" "}" $
    "\"title\":" <> TL.pack (show t) <> ",\"score\":" <> TL.pack (show s)
-}
newPost appState poster title content = do
  modifyMVar_ appState $ \(AppState {..}) -> do
    let post = Post {poster, title, content, votes = M.empty, idNum = uid}
    return $ AppState {uid = uid + 1, postDB = M.insert uid post postDB}
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

writePosts appState = writeFile "posts" . show =<< readMVar appState

main :: IO ()
main = do
  appState <- newMVar =<< getAppState
  key <- getDefaultKey

  void $ installHandler keyboardSignal 
    (Catch (writePosts appState)) Nothing

  let vote dir = do
        postId <- param "postId"
        user   <- loggedInUserErr key
        v'     <- liftIO . modifyMVar appState $ \s@(AppState{..}) ->
          let v  = fromMaybe None $ M.lookup user . votes =<< M.lookup postId postDB
              v' = updateVote dir v
              db' = M.alter (fmap (\p -> p { votes = M.insert user v' (votes p)})) postId postDB
          in
          return (s {postDB = db'}, v')
        let txt = Data.Aeson.encode $ SetVote postId v'
        liftIO $ print txt
        text $ TL.decodeUtf8 txt

  scotty 3000 $ do
    middleware $ staticPolicy (noDots <> addBase "static")

    post "/login" $ do
      cnetId   <- param "username"
      password <- param "password"
      liftIO (authorize cnetId password) >>= \case
        Nothing -> raise "password no work"
        Just () -> do
          c <- liftIO $ encryptIO key (BC8.pack cnetId)
          setHeader "Set-Cookie" (TL.decodeUtf8 $ BL.fromStrict c)
          redirect "/"

    get "/" $ do
      setHeader "Content-Type" "text/html"
      file "static/index.html"

    get "/posts" $ do
      setHeader "Access-Control-Allow-Origin" "*"
      user <- loggedInUserErr key
      ps <- liftIO (postsForUser user <$> readMVar appState)
      liftIO (print =<< readMVar appState)
      liftIO (TL.putStrLn ps)
      text ps

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

