{-# LANGUAGE RankNTypes #-}

module Authentication.Repository.UserRepository where

import Authentication.Domain.Models (GetSavedUserByLogin, CreateUserBySignup)

data UserRepository m = UserRepository
  { getUserByLogin :: GetSavedUserByLogin m,
    createUserBySignup :: CreateUserBySignup m
  }
