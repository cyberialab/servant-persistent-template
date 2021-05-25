{-# LANGUAGE RankNTypes #-}

module Authentication.Repository.UserRepository where

import Authentication.Domain.Models (AuthenticatedUser, Login, SignUp)
import Base.Error (Err)
import Prelude

data UserRepository m = UserRepository
  { getUserByLogin :: GetUserByLogin m,
    createUserBySignup :: CreateUserBySignup m
  }

data ErrInsertUser = InsertUserConflict
  deriving (Show, Eq)

type CreateUserBySignup m = Monad m => SignUp -> m (Either (Err ErrInsertUser) AuthenticatedUser)

data ErrGetUserByLogin
  = UserDoesNotExist
  | ReplicatedUser
  deriving (Show, Eq)

type GetUserByLogin m = Monad m => Login -> m (Either (Err ErrGetUserByLogin) AuthenticatedUser)
