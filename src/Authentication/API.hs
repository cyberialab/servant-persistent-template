{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

{-# OPTIONS_GHC -fno-warn-deprecations #-}

module Authentication.API where

import Database.Persist.Postgresql (ConnectionPool)

import Authentication.Domain.Models
import Authentication.Domain.Commands.SignUpUser ( signUpUser )
import Authentication.Domain.Queries.LoginUser ( loginUser )

import Authentication.Repository.Persistent.Postgres
import Servant
import Servant.Auth.Server
import Control.Monad.Reader (MonadIO, liftIO)
import Control.Monad.Logger ( logDebugNS )

import Base.Error ( Err(SpecificErr))
import Data.Password.Bcrypt (checkPassword, PasswordCheck(..), mkPassword)

import Config

type AcceptHeader returnContent =
    Headers
        '[ Header "Set-Cookie" SetCookie
         , Header "Set-Cookie" SetCookie
         ]
         returnContent

type RegistrationAppT m = AppT m (AcceptHeader NoContent)

type AuthenticationAPIAction actionPayload m = CookieSettings -> JWTSettings -> actionPayload -> RegistrationAppT m

type LoginAPI =
    "login" 
        :> ReqBody '[JSON] Login
        :> Verb 'POST 204 '[JSON] (AcceptHeader NoContent)

type RegisterAPI = "signup" :> ReqBody '[JSON] SignUp :> Verb 'POST 204 '[JSON] (AcceptHeader NoContent)

type AuthenticationAPI =
    LoginAPI :<|> RegisterAPI

_err401 :: ServerError
_err401 = err401 { errBody = "Error logging user in, please verify credentials" }

getAuthenticatedCookies :: (MonadIO m) => AuthenticationAPIAction AuthenticatedUser m
getAuthenticatedCookies cookieSettings jwtSettings user = do
  mApplyCookies <- liftIO $ acceptLogin cookieSettings jwtSettings user
  
  case mApplyCookies of
    Nothing           -> throwError $ _err401
    Just applyCookies -> return $ applyCookies NoContent
    
checkLoginPassword :: (MonadIO m) => CheckPassword m
checkLoginPassword login (SavedUser username password) = do
  let pass = mkPassword $ loginPassword login
  let hashedPass = password
  let checkedPassword = checkPassword pass hashedPass
  case checkedPassword of
    PasswordCheckFail -> return $ Left (SpecificErr WrongPassword)
    PasswordCheckSuccess -> return $ Right $ AuthenticatedUser username 
  
loginAPI :: (MonadIO m) => ConnectionPool -> AuthenticationAPIAction Login m
loginAPI pool cookieSettings jwtSettings loginInformation = do
  logDebugNS "Authentication" "signUpAPI"
  authUser <- loginUser checkLoginPassword (getUserPostgres pool) loginInformation
  case authUser of
    Authenticated user -> getAuthenticatedCookies cookieSettings jwtSettings user
    _ -> throwError _err401
      
signUpAPI :: (MonadIO m) => ConnectionPool -> AuthenticationAPIAction SignUp m
signUpAPI pool cookieSettings jwtSettings signUp = do
  logDebugNS "Authentication" "signUpAPI"
  authUser <- signUpUser (createUserPostgres pool) signUp
  let _err401 = err401 { errBody = "Error logging user in, please verify credentials" }
  case authUser of
    Authenticated user -> getAuthenticatedCookies cookieSettings jwtSettings user
    _ -> throwError _err401
    
authenticationAPI :: Proxy AuthenticationAPI
authenticationAPI = Proxy

authenticationServer :: (MonadIO m) => CookieSettings -> JWTSettings -> ConnectionPool -> ServerT AuthenticationAPI (AppT m)
authenticationServer cs jwts pool = loginAPI pool cs jwts :<|> signUpAPI pool cs jwts
