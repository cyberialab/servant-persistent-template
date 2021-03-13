

{-# OPTIONS_GHC -fno-warn-orphans #-}

module App where

import Network.Wai
import Web.HttpApiData (parseBoundedTextData)

import Config

import WordsCount.API
import WordsCount.Domain

import Servant
    ( Proxy(..), serve, FromHttpApiData(parseUrlPiece), Server )

-- * data

--This instance is necessary for the QueryParam definition as it needs some way to parse the text to the specified type
instance FromHttpApiData SortBy where parseUrlPiece = parseBoundedTextData

-- * api

-- This is the API definition
type API = WordsCountAPI

api :: Proxy API
api = Proxy

-- Creates server definition for API, which is the one that adds the logic to the typed specification
server :: Server API
server = countWordsAPI

-- Creates IO application from API definition and serrver
mkApp :: Config -> Application
mkApp _ = serve api server
