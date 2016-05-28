{-# LANGUAGE OverloadedStrings #-}

module Network.Authenticate.ETradeOAuth
where

import           Data.ByteString         as BS
import           Data.ByteString.Char8   as C8 (pack, unpack)
import           Data.List               as ML
import           Data.Maybe
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS
import           Network.HTTP.Types.URI
import           System.Environment
import           System.IO               ()
import           Web.Authenticate.OAuth
import           Web.Browser

authToken :: ByteString
authToken = "oauth_token"

-- OAuth Object to use when communicating with
-- ETrade developer API. Also, it must define
-- the two fields: oauthConsumerKey and oauthConsumerSecret
-- to be able to stablish a succesful communication.
-- For more information:
-- https://developer.etrade.com/ctnt/dev-portal/getArticleByCategory?category=Documentation
oa :: OAuth
oa = newOAuth {
    oauthServerName      = "ETrade"
  , oauthRequestUri      = "https://etws.etrade.com/oauth/request_token"
  , oauthAccessTokenUri  = "https://etws.etrade.com/oauth/access_token"
  , oauthAuthorizeUri    = "https://us.etrade.com/e/t/etws/authorize"
  , oauthSignatureMethod = HMACSHA1
  , oauthVersion         = OAuth10a
  , oauthConsumerKey     = undefined
  , oauthConsumerSecret  = undefined
  , oauthCallback        = Just "oob"
  }

-- We use a tlsManager to communicate using HTTPS
mgr :: IO Manager
mgr = newManager tlsManagerSettings

-- OAuth can construct automatically the authorization
-- URL, however ETrade uses another structure, which
-- is depicted as follows:
-- https://us.etrade.com/e/t/etws/authorize?key={oauth_consumer_key}&token={oauth_token}
etradeAuthUri :: OAuth -> Credential -> ByteString
etradeAuthUri oa crd = BS.concat [authUri, "?key=", consumerKey, "&token=", token]
    where
        authUri = C8.pack $ oauthAuthorizeUri oa
        token = urlEncode True $ findAuth $ unCredential crd
        consumerKey = urlEncode True $ oauthConsumerKey oa

-- Helper function to find the OAuth token out of a temporal credential
findAuth :: [(ByteString, ByteString)] -> ByteString
findAuth x = snd $ fromJust findAction
    where
        findFunction a = authToken == fst a
        findAction = ML.find findFunction x

-- This will innitiate the OAuth process, using an OAuth object and
-- a TLS Manager to asking for a temporary credential, with which it
-- will redirect the user to a ETrade webpage in which access is
-- authorized and a confirmation string is provided
doInitialAuth :: OAuth -> Manager -> IO Credential
doInitialAuth oa m = do
    temp <- getTemporaryCredential oa m
    let authURL = etradeAuthUri oa temp
    openBrowser $ C8.unpack authURL
    return temp

-- With the confirmation string in place, we are able to as for an
-- access token to the platform which we will use subsequently to
-- make all request
doAccessAuth :: OAuth -> Manager -> ByteString -> Credential -> IO Credential
doAccessAuth oa m verifier tempCred = getAccessToken oa cred m
    where cred = injectVerifier verifier tempCred

main :: IO Credential
main = do
    print "This script will request an OAuth Token and Open the browser"
    m <- mgr
    temp <- doInitialAuth oa m
    print "Provide the Confirmation String"
    confirm <- BS.getLine
    if BS.null confirm
        then do print "Not a valid answer";
                return $ newCredential "" ""
        else do cred <- doAccessAuth oa m confirm temp
                print "OAuth to continue requests:"
                print cred
                return cred
