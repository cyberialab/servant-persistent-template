{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Authentication.Domain.Commands.SignUpUser where

import Authentication.Domain.Models (AuthenticatedUser (..), SignUp)
import Authentication.Repository.UserRepository (CreateUserBySignup)
import Servant.Auth.Server

matchAuthenticatedUser :: (Monad m) => SignUp -> CreateUserBySignup m -> m (AuthResult AuthenticatedUser)
matchAuthenticatedUser signUp createUserBySignup = do
  authUser <- createUserBySignup signUp 
  case authUser of
    Left _ ->
      return Indefinite
    Right user ->
      return $ Authenticated user

