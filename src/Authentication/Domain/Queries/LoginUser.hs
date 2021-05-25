{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Authentication.Domain.Queries.LoginUser where

import Authentication.Domain.Models (AuthenticatedUser (..), Login)
import Authentication.Repository.UserRepository (ErrGetUserByLogin (ReplicatedUser, UserDoesNotExist), GetUserByLogin)
import Base.Error (Err (AnyErr, SpecificErr))
import Servant.Auth.Server

matchAuthenticatedUser :: (Monad m) => Login -> GetUserByLogin m -> m (AuthResult AuthenticatedUser)
matchAuthenticatedUser login getUserByLogin = do
  authUser <- getUserByLogin login
  case authUser of
    Left (SpecificErr err) ->
      case err of
        UserDoesNotExist -> return NoSuchUser
        ReplicatedUser -> return Indefinite
    Left AnyErr ->
      return Indefinite
    Right user ->
      return $ Authenticated user
