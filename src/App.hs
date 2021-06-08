{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TypeOperators   #-}

module App where

import Control.Monad.Reader (runReaderT, MonadIO)

import Network.Wai
import Web.HttpApiData (parseBoundedTextData)

import Config
import Database.Persist.Postgresql (ConnectionPool)

import Authentication.API
import Authentication.Domain.Models (AuthenticatedUser)

import WordsCount.API
import WordsCount.Domain

import Servant
import Servant.Auth.Server

--import Servant.Auth.Server.SetCookieOrphan ()
-- * data

--This instance is necessary for the QueryParam definition as it needs some way to parse the text to the specified type
instance FromHttpApiData SortBy where parseUrlPiece = parseBoundedTextData

-- * api

-- This is the API definition

api :: Proxy API
api = Proxy

context :: Proxy '[CookieSettings, JWTSettings]
context = Proxy 

type Authorized = Auth '[Cookie] AuthenticatedUser
type API = WordsCountAPI :<|> AuthenticationAPI

-- * general

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

appToServer :: Config -> Server API
appToServer cfg = hoistServerWithContext api context (convertApp cfg) $ server (configPool cfg) (configJWTSettings cfg)

-- Creates server definition for API, which is the one that adds the logic to the typed specification
server :: (MonadIO m) => ConnectionPool -> JWTSettings -> ServerT API (AppT m)
server pool jwtSettings = do
  countWordsAPI :<|> authenticationServer defaultCookieSettings jwtSettings pool 

cookieSettings :: CookieSettings
cookieSettings = defaultCookieSettings { cookieIsSecure = NotSecure, cookieSameSite = SameSiteStrict, cookieXsrfSetting = Nothing }

-- Creates IO application from API definition and serrver
mkApp :: Config -> Application
mkApp config = do
  let ctx = cookieSettings :. (configJWTSettings config) :. EmptyContext
  serveWithContext api ctx (appToServer config)
