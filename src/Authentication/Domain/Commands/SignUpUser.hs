{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Authentication.Domain.Commands.SignUpUser where

import Authentication.Domain.Models (AuthenticatedUser (..), SignUp, CreateUserBySignup, ErrInsertUser)
import Servant.Auth.Server
import Base.Error (Err)
import Control.Monad ((>=>))

validateUser :: (Monad m) => Either (Err ErrInsertUser) AuthenticatedUser -> m (AuthResult AuthenticatedUser)
validateUser authUser = 
  case authUser of
    Left _ ->
      return Indefinite
    Right user ->
      return $ Authenticated user


signUpUser :: (Monad m) => CreateUserBySignup m -> SignUp -> m (AuthResult AuthenticatedUser)
signUpUser createUserBySignup = do
   createUserBySignup >=> validateUser  
