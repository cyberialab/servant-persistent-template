{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Authentication.Domain.Models where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.Auth.Server
import Base.Error (Err)
import Data.Password.Bcrypt (Bcrypt, PasswordHash(..))

type Password = Text
type Username = Text
type HashedPassword = PasswordHash Bcrypt

newtype AuthenticatedUser = AuthenticatedUser Username
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

data SavedUser = SavedUser Username HashedPassword
  deriving (Show, Generic)

data SignUp = SignUp { signUpUsername :: !Username, signUpPassword :: !Password }
   deriving (Show, Generic)

instance ToJSON SignUp
instance FromJSON SignUp

data Login = Login { loginUsername :: !Username, loginPassword :: !Password }
   deriving (Show, Generic)

instance ToJSON Login
instance FromJSON Login

type Authorized = Auth '[Cookie, JWT] AuthenticatedUser

data ErrInsertUser = InsertUserConflict
  deriving (Show, Eq)

type CreateUserBySignup m = Monad m => SignUp -> m (Either (Err ErrInsertUser) AuthenticatedUser)

data ErrGetUserByLogin
  = UserDoesNotExist
  | ReplicatedUser
  | WrongPassword
  deriving (Show, Eq)

type GetSavedUserByLogin m = Login -> m (Either (Err ErrGetUserByLogin) SavedUser)
type CheckPassword m = Login -> SavedUser -> m (Either (Err ErrGetUserByLogin) AuthenticatedUser)
