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
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Database where

import Control.Monad.Reader (MonadIO, MonadReader, asks, liftIO)
import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Database.Persist.Quasi
import Database.Persist.Sql (SqlPersistT, runMigration, runSqlPool)
import Data.Text (Text)
import Data.Typeable (Typeable)
import Say

import Config (Config, configPool)

share [mkPersist sqlSettings, mkMigrate "migrateAll"]
  $(persistManyFileWith lowerCaseSettings ["config/models/User.persistentmodels"])

doMigrations :: SqlPersistT IO ()
doMigrations = do
  liftIO $ say "Database.doMigrations"
  runMigration migrateAll
  liftIO $ say "Migrations ran successfuly"

runDb :: (MonadReader Config m, MonadIO m) => SqlPersistT IO b -> m b
runDb query = do
  pool <- asks configPool
  liftIO $ runSqlPool query pool
