{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

module Database where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Quasi
import Data.Text (Text)
import Say

import Data.Password.Bcrypt (Bcrypt, PasswordHash(..))
import Data.Password.Instances ()

import Config (Config, configPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistManyFileWith lowerCaseSettings [
      "config/models/User.persistentmodels"
    ]
  )

doMigrations :: SqlPersistT IO ()
doMigrations = do
  liftIO $ say "Database.doMigrations"
  runMigration migrateAll
  liftIO $ say "Migrations ran successfuly"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
