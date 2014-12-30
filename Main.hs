{-# LANGUAGE RecordWildCards, OverloadedStrings, ViewPatterns #-}
module Main where

import LDAP
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Text.Lazy as T
import Data.Monoid
import Web.Scotty
import qualified Network.Wai as W

data Credentials = Credentials { username :: String, password :: String}
type OrError = Either String

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

{-
data Post = Post { title :: String, score :: Int }
  deriving (Show, Read)
-}

type Post = (String, Int)
type PostDB = M.Map String Int

readPostDB :: IO PostDB
readPostDB = read <$> readFile "posts"


postsToJSON :: PostDB -> T.Text
postsToJSON posts = "[" <> T.intercalate "," (map postToJSON (M.toList posts)) <> "]" where
  postToJSON (t, s) = "{" <> "\"title\":" <> T.pack (show t) <> ",\"score\":" <> T.pack (show s) <> "}"

main :: IO ()
main = do
  postsVar <- newMVar =<< readPostDB
  scotty 3000 $ do
    get "/posts" $
      text . postsToJSON =<< liftIO (readMVar postsVar)

    get "*" $
      text "Hi!"

