{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS8
import Data.Maybe (fromMaybe)
import Data.Monoid ((<>))
import Network.HTTP.Client (urlEncodedBody)
import System.Environment (getEnv)
import System.IO (hFlush, stdout)
import Web.Authenticate.OAuth as OA
import Web.Twitter.Conduit hiding (lookup)

import Debug.Trace

authorize oauth mgr rw = do
  cred <- OA.getTemporaryCredential' access_type oauth mgr
  putStrLn $ "Please, go to navigate to: " ++ OA.authorizeUrl oauth cred
  putStr $ "PIN: "
  hFlush stdout
  pin <- BS8.getLine
  OA.getAccessToken oauth (OA.insert "oauth_verifier" pin cred) mgr

 where
  access_type = urlEncodedBody [("x_auth_access_type", if rw then "write" else "read")]

main :: IO ()
main = do
  consumer_key    <- getEnv "FUNNY_CONSUMER_KEY"
  consumer_secret <- getEnv "FUNNY_CONSUMER_SECRET"

  mgr <- newManager tlsManagerSettings

  let oauth = twitterOAuth { oauthConsumerKey    = BS8.pack consumer_key
                           , oauthConsumerSecret = BS8.pack consumer_secret
                           , oauthCallback = Just "oob"
                           }

  putStrLn $ "Getting authorization for _my_ account"
  Credential cred <- authorize oauth mgr True

  putStrLn $ "Getting authorization for _moderator_ account"
  Credential modCred <- authorize oauth mgr False

  BS8.putStrLn . BS8.intercalate "\n" $
    [ "export FUNNY_CONSUMER_KEY=\"" <> oauthConsumerKey oauth <> "\""
    , "export FUNNY_CONSUMER_SECRET=\"" <> oauthConsumerSecret oauth <> "\""
    , "export FUNNY_AUTH_TOKEN=\"" <> fromMaybe "" (lookup "oauth_token" cred) <> "\""
    , "export FUNNY_AUTH_SECRET=\"" <> fromMaybe "" (lookup "oauth_token_secret" cred) <> "\""
    , ""
    , "export FUNNY_MODERATOR_TOKEN=\"" <> fromMaybe "" (lookup "oauth_token" modCred) <> "\""
    , "export FUNNY_MODERATOR_SECRET=\"" <> fromMaybe "" (lookup "oauth_token_secret" modCred) <> "\""
    ]
