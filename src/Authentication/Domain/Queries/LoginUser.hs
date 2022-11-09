{-# LANGUAGE RankNTypes #-}

module Authentication.Domain.Queries.LoginUser where

import Authentication.Domain.Models

import Base.Error (Err (AnyErr, SpecificErr))
import Servant.Auth.Server

validateUser :: (Monad m) => Either (Err ErrGetUserByLogin) AuthenticatedUser -> m (AuthResult AuthenticatedUser)
validateUser authUser =
  case authUser of
    Left (SpecificErr err) ->
      case err of
        UserDoesNotExist -> return NoSuchUser
        _ -> return Indefinite
    Left AnyErr ->
      return Indefinite
    Right user ->
      return $ Authenticated user

loginUser :: (Monad m) => CheckPassword m -> GetSavedUserByLogin m -> Login -> m (AuthResult AuthenticatedUser)
loginUser checkPassword getSavedUserByLogin login = do
  savedUser <- getSavedUserByLogin login
  case savedUser of
    Left getUserError -> validateUser (Left getUserError)
    Right user -> do
      checkedUser <- checkPassword login user
      validateUser checkedUser
         
  
