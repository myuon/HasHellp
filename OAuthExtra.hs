module OAuthExtra 
       ( githubUrl
       , authGithub
       ) where

import Prelude
import Yesod.Auth
import Web.Authenticate.OAuth
import Data.Maybe
import Control.Arrow ((***))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.ByteString (ByteString)
import Yesod.Auth.OAuth

githubUrl :: AuthRoute
githubUrl = oauthUrl "github"

authGithub :: YesodAuth m
              => ByteString
              -> ByteString
              -> AuthPlugin m
authGithub key secret = 
  authOAuth (newOAuth { oauthServerName      = "github"
                      , oauthRequestUri      = "https://api.github.com/user"
                      , oauthAccessTokenUri  = "https://github.com/login/oauth/access_token"
                      , oauthAuthorizeUri    = "https://github.com/login/oauth/authorize"
                      , oauthSignatureMethod = HMACSHA1
                      , oauthConsumerKey     = key
                      , oauthConsumerSecret  = secret
                      , oauthVersion         = OAuth10
                      })
  extractCreds
  where
    extractCreds (Credential dic) = do
      let crId = decodeUtf8With lenientDecode $ fromJust $ lookup "code" dic
      return $ Creds "github" crId $ map (bsToText *** bsToText) dic

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
