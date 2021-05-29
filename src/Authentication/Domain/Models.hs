{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Authentication.Domain.Models where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Servant.Auth.Server

newtype AuthenticatedUser = AuthenticatedUser
  { authUsername :: Username }
  deriving (Show, Generic)

instance ToJSON AuthenticatedUser
instance FromJSON AuthenticatedUser
instance ToJWT AuthenticatedUser
instance FromJWT AuthenticatedUser

type Password = Text
type Username = Text

data SignUp = SignUp { signUpUsername :: !Username, signUpPassword :: !Text }
   deriving (Show, Generic)

instance ToJSON SignUp
instance FromJSON SignUp

data Login = Login { loginUsername :: !Username, loginPassword :: !Text }
   deriving (Show, Generic)

instance ToJSON Login
instance FromJSON Login

type Authorized = Auth '[Cookie] AuthenticatedUser
