{-# LANGUAGE OverloadedStrings #-}

module Main where

import Conduit
import Control.Lens ((^.))
import Control.Monad (guard)
import Control.Monad.Trans.Resource (runResourceT)
import qualified Data.ByteString.Char8 as BS8
import System.Environment (getEnv)
import Web.Twitter.Conduit
import Web.Twitter.Types (Event(Event), StreamingAPI(SEvent))
import Web.Twitter.Types.Lens

import Api (unretweetId)


createTwInfo :: String -> String -> String -> String -> TWInfo
createTwInfo consumer_key consumer_secret token secret = setCredential tokens creds def
 where
  tokens = twitterOAuth
    { oauthConsumerKey    = BS8.pack consumer_key
    , oauthConsumerSecret = BS8.pack consumer_secret
    }
  creds = Credential
    [ ("oauth_token", BS8.pack token)
    , ("oauth_token_secret", BS8.pack secret)
    ]

watchLikes modId srcId twInfo mgr = concatMapC event .| mapM_C process
 where
  event (SEvent e) = Just e
  event _          = Nothing

  process e | e ^. evEvent == "favorite"   = assertUsers *> liftIO rt *> pure ()
            | e ^. evEvent == "unfavorite" = assertUsers *> liftIO unrt *> pure ()
            | otherwise                    = pure ()
    where
      assertUsers =
        let ETUser srcUser = e ^. evSource
            ETUser tgtUser = e ^. evTarget
        in guard $ (srcUser ^. userId == modId) && (tgtUser ^. userId == srcId)
      twId = let Just (ETStatus s) = e ^. evTargetObject in s ^. statusId
      rt = call twInfo mgr $ retweetId twId
      unrt = call twInfo mgr $ unretweetId twId


main :: IO ()
main = do
  consumer_key    <- getEnv "FUNNY_CONSUMER_KEY"
  consumer_secret <- getEnv "FUNNY_CONSUMER_SECRET"
  auth_token  <- getEnv "FUNNY_AUTH_TOKEN"
  auth_secret <- getEnv "FUNNY_AUTH_SECRET"

  sourceName <- getEnv "FUNNY_SOURCE"

  moderator_token  <- getEnv "FUNNY_MODERATOR_TOKEN"
  moderator_secret <- getEnv "FUNNY_MODERATOR_SECRET"

  mgr <- newManager tlsManagerSettings

  let modTwInfo = createTwInfo consumer_key consumer_secret moderator_token moderator_secret
  let twInfo = createTwInfo consumer_key consumer_secret auth_token auth_secret

  modUser <- call modTwInfo mgr $ accountVerifyCredentials
  let modId = modUser ^. userId

  srcUser <- call twInfo mgr $ usersShow (ScreenNameParam sourceName)
  let srcId = srcUser ^. userId

  runResourceT $ do
    src <- stream modTwInfo mgr userstream
    src $$+- watchLikes modId srcId twInfo mgr
