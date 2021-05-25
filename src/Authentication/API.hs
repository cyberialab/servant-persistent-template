{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Authentication.API where

import Database.Persist.Postgresql (ConnectionPool)

import Authentication.Domain.Models (Login, SignUp, AuthenticatedUser)
import Authentication.Repository.Persistent.Postgres
import Servant
import Servant.Auth.Server
import Control.Monad.Reader (MonadIO, MonadReader, asks)
import Control.Monad.Logger ( logDebugNS )

import Config

type AcceptHeader returnContent =
    Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
         returnContent

type LoginAPI =
    "login" 
        :> ReqBody '[JSON] Login
        :> Post '[JSON] AuthenticatedUser

type RegisterAPI = "signup" :> ReqBody '[JSON] SignUp :> Post '[JSON] AuthenticatedUser

type AuthenticationAPI =
    LoginAPI :<|> RegisterAPI

loginAPI :: (MonadIO m) => ConnectionPool -> Login -> AppT m AuthenticatedUser
loginAPI pool login = do
  logDebugNS "Authentication" "signUpAPI"
  authUser <- getUserPostgres pool login
  case authUser of
    Left _ -> throwError $ err401 { errBody = "Error logging user in, please verify credentials" }
    Right user -> return user

signUpAPI :: (MonadIO m) => ConnectionPool -> SignUp -> AppT m AuthenticatedUser
signUpAPI pool signUp = do
  logDebugNS "Authentication" "signUpAPI"
  authUser <- createUserPostgres pool signUp
  case authUser of
    Left _ -> throwError $ err401 { errBody = "Error signing up user, please verify user information." }
    Right user -> return user

authenticationAPI :: Proxy AuthenticationAPI
authenticationAPI = Proxy

authenticationServer :: (MonadIO m) => ConnectionPool -> ServerT AuthenticationAPI (AppT m)
authenticationServer pool = loginAPI pool :<|> signUpAPI pool
