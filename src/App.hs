{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module App where

import Control.Monad.Reader (runReaderT, MonadReader, MonadIO)

import Network.Wai
import Web.HttpApiData (parseBoundedTextData)

import Config
import Database.Persist.Postgresql (ConnectionPool)

import Authentication.API

import WordsCount.API
import WordsCount.Domain

import Servant

-- * data

--This instance is necessary for the QueryParam definition as it needs some way to parse the text to the specified type
instance FromHttpApiData SortBy where parseUrlPiece = parseBoundedTextData

-- * general

convertApp :: Config -> AppT IO a -> Handler a
convertApp cfg appt = Handler $ runReaderT (runApp appt) cfg

appToServer :: Config -> Server API
appToServer cfg = hoistServer api (convertApp cfg) (server $ configPool cfg)

-- * api

-- This is the API definition
type API = WordsCountAPI :<|> AuthenticationAPI

api :: Proxy API
api = Proxy

-- Creates server definition for API, which is the one that adds the logic to the typed specification
server :: (MonadIO m) => ConnectionPool -> ServerT API (AppT m)
server pool = countWordsAPI :<|> authenticationServer pool

-- Creates IO application from API definition and serrver
mkApp :: Config -> Application
mkApp config = serve api (appToServer config)
