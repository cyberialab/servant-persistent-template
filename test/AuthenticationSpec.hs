{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module AuthenticationSpec where

import Test.Hspec

import Test.QuickCheck
import Test.QuickCheck.Utf8
import Test.QuickCheck.Gen.Unsafe (promote)

import qualified Authentication.Domain.Commands.SignUpUser as SignUp
import Authentication.Domain.Queries.LoginUser as Login
import Authentication.Domain.Models
import Servant.Auth.Server
import Base.Error
import Data.Password.Bcrypt (PasswordHash(..))

instance Arbitrary AuthenticatedUser where
  arbitrary = AuthenticatedUser <$> genValidUtf8

instance Arbitrary HashedPassword where
  arbitrary = PasswordHash <$> genValidUtf8

-- Login process generators

loginErrorGen :: Gen ErrGetUserByLogin
loginErrorGen = elements [UserDoesNotExist, ReplicatedUser, WrongPassword]

instance Arbitrary Login where
  arbitrary = Login <$> genValidUtf8 <*> genValidUtf8

instance Arbitrary SavedUser where
  arbitrary = SavedUser <$> genValidUtf8 <*> arbitrary

instance Arbitrary (Err ErrGetUserByLogin) where
  arbitrary = SpecificErr <$> loginErrorGen

type MockedLoginFunctionType = Login -> Maybe (Either (Err ErrGetUserByLogin) SavedUser)
promotedLoginFunction :: Gen MockedLoginFunctionType
promotedLoginFunction = promote $ \_ -> arbitrary :: Gen (Maybe (Either (Err ErrGetUserByLogin) SavedUser))

type MockedCheckPasswordFunctionType = Login -> SavedUser -> Maybe (Either (Err ErrGetUserByLogin) AuthenticatedUser)
promotedLoginUserFunction :: Gen MockedCheckPasswordFunctionType
promotedLoginUserFunction = promote $ \_ -> promote $ \_ -> arbitrary :: Gen (Maybe (Either (Err ErrGetUserByLogin) AuthenticatedUser))

instance CoArbitrary Login where
  coarbitrary _ genb = genb

instance CoArbitrary SavedUser where
  coarbitrary _ genb = genb

instance Show MockedLoginFunctionType where
  show _ = "Mocked login function"

instance Show MockedCheckPasswordFunctionType where
  show _ = "Mocked check password function"
  
-- SignUp process generators

insertErrorGen :: Gen ErrInsertUser
insertErrorGen = elements [InsertUserConflict]

type MockedSignUpFunctionType = SignUp -> Maybe (Either (Err ErrInsertUser) AuthenticatedUser)
promotedSignUpMockedFunction :: Gen MockedSignUpFunctionType
promotedSignUpMockedFunction = promote $ \_ -> arbitrary :: Gen (Maybe (Either (Err ErrInsertUser) AuthenticatedUser))

instance Arbitrary SignUp where
  arbitrary = SignUp <$> genValidUtf8 <*> genValidUtf8

instance CoArbitrary SignUp where
  coarbitrary _ genb = genb

instance Arbitrary (Err ErrInsertUser) where
  arbitrary = SpecificErr <$> insertErrorGen

instance Show MockedSignUpFunctionType where
  show _ = "Mocked signUp function"
  
-- Maybe is used as a hack for mocking the Monad instance wrapping the result. Probably there's a better way of doing this.
spec :: Spec
spec = do
  describe "Authentication.Domain.Commands.SignUpUser.validateUser" $ do
    it "returns if user is Authenticated depending on command result" $ \ _ -> do 
      forAll (arbitrary :: Gen (Either (Err ErrInsertUser) AuthenticatedUser)) $ \genAuthRes -> do
        case genAuthRes of
          Left err -> do
            result <- (SignUp.validateUser $ Left err) :: Maybe (AuthResult AuthenticatedUser) 
            case result of
              Indefinite -> return True
              _ -> return False
          Right user -> do
            result <- SignUp.validateUser $ Right user :: Maybe (AuthResult AuthenticatedUser)
            case result of
              Authenticated _ -> return True
              _ -> return False
              
  describe "Authentication.Domain.Commands.SignUpUser.signUpUser" $ do
    it "returns valid Authentication result depending on signUpFunction and signUp information" $ \ _ -> do
      forAll (arbitrary :: Gen SignUp) $ \signUp -> do
        forAll (arbitrary :: Gen MockedSignUpFunctionType) $ \signUpFunction -> do
          signUpUserResult <- signUpFunction signUp
          
          case signUpUserResult of
            Left _ -> do
              result <- (SignUp.signUpUser signUpFunction signUp)
              case result of
                Authenticated _ -> return False
                _ -> return True
            Right _ -> do
              result <- (SignUp.signUpUser signUpFunction signUp)
              case result of
                Authenticated _ -> return True
                _ -> return False
              
  describe "Authentication.Domain.Queries.LoginUser.validateUser" $ do
    it "returns if user is Authenticated depending on user query result" $ \ _ -> do 
      forAll (arbitrary :: Gen (Either (Err ErrGetUserByLogin) AuthenticatedUser)) $ \genAuthRes -> do
        case genAuthRes of
          Left (SpecificErr UserDoesNotExist) -> do
            result <- (Login.validateUser $ Left (SpecificErr UserDoesNotExist)) :: Maybe (AuthResult AuthenticatedUser)
            case result of
              NoSuchUser -> return True
              _ -> return False
          Left (SpecificErr err) -> do
            result <- (Login.validateUser $ Left (SpecificErr err)) :: Maybe (AuthResult AuthenticatedUser)
            case result of
              Indefinite -> return True
              _ -> return False
          Left AnyErr -> do
            result <- (Login.validateUser $ Left AnyErr) :: Maybe (AuthResult AuthenticatedUser)
            case result of
              Indefinite -> return True
              _ -> return False
          Right user -> do
            result <- Login.validateUser $ Right user :: Maybe (AuthResult AuthenticatedUser)
            case result of
              Authenticated _ -> return True
              _ -> return False

  describe "Authentication.Domain.Queries.LoginUser.loginUser" $ do
    it "returns valid Authentication result depending on checkPassword, loginFunction and login information" $ \ _ -> do
      forAll (arbitrary :: Gen Login) $ \login -> do
        forAll (arbitrary :: Gen SavedUser) $ \savedUser -> do
          forAll (arbitrary :: Gen MockedCheckPasswordFunctionType) $ \checkPassword -> do
            forAll (arbitrary :: Gen  MockedLoginFunctionType) $ \loginFunction -> do
              genPassVerification <- checkPassword login savedUser
              case genPassVerification of
                Left _ -> do
                  result <- (Login.loginUser checkPassword loginFunction login)
                  case result of
                    Authenticated _ -> return False
                    _ -> return True
                Right _ -> do
                  loginUserResult <- loginFunction login
                  case loginUserResult of
                    Left _ -> do
                      result <- (Login.loginUser checkPassword loginFunction login)
                      case result of
                        Authenticated _ -> return False
                        _ -> return True
                    Right _ -> do
                      result <- (Login.loginUser checkPassword loginFunction login)
                      case result of
                        Authenticated _ -> return True
                        _ -> return False
     
