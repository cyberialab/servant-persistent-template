{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Authentication.Repository.Persistent.Postgres where

import Authentication.Repository.UserRepository
import Authentication.Domain.Models

import Data.Password.Types (mkPassword) 
import Data.Password.Bcrypt (hashPassword)

import Database.Persist (selectList, insertBy)
import Database.Persist.Postgresql (Entity(..), (==.), runSqlPool, ConnectionPool)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger ( logDebugNS, MonadLogger(..))

import Database as DB
    ( EntityField(UserIdent), User(User, userIdent, userPassword) )
    
import Base.Error (Err( SpecificErr))

mapUserToAuthenticatedUser :: User -> AuthenticatedUser
mapUserToAuthenticatedUser user = AuthenticatedUser (userIdent user)

mapUserToSavedUser :: User -> SavedUser
mapUserToSavedUser user = SavedUser (userIdent user) (userPassword user)

getUserPostgres :: (MonadIO m, MonadLogger m) => ConnectionPool -> GetSavedUserByLogin m
getUserPostgres pool login = do
    logDebugNS "web" "getUserPostgres"

    let query = selectList [UserIdent ==. loginUsername login] []
    users <- liftIO $ runSqlPool query pool
    
    case length users of 
        0 -> return $ Left (SpecificErr UserDoesNotExist)
        1 -> return $ Right $ head $ map (mapUserToSavedUser . entityVal) users
        _ -> return $ Left (SpecificErr ReplicatedUser)

createUserPostgres :: (MonadIO m, MonadLogger m) => ConnectionPool -> CreateUserBySignup m
createUserPostgres pool signUp = do
    logDebugNS "web" "getUserPostgres"
    hashedPsw <- hashPassword $ mkPassword $ signUpPassword signUp
    let username = signUpUsername signUp
    let user = User username hashedPsw
    let query = insertBy $ user
    result <- liftIO $ runSqlPool query pool
    
    case result of
        Left _ -> return $ Left (SpecificErr InsertUserConflict)
        Right _ -> return $ Right $ mapUserToAuthenticatedUser user 

postgresUserRepository :: (MonadIO m, MonadLogger m) => ConnectionPool -> UserRepository m
postgresUserRepository pool = UserRepository {
    getUserByLogin = getUserPostgres pool,
    createUserBySignup = createUserPostgres pool
}
