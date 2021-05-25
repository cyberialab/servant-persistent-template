{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Util.Database.Persist.Postgresql.Json where

import Data.Aeson
import Database.Persist
import Database.Persist.Sql

import Data.ByteString.Lazy (toStrict, fromStrict)
import Data.Bifunctor (first)

import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import qualified Data.Text as T

newtype Json
  = Json Value
  deriving
    ( Show
    , Eq
    , FromJSON
    , ToJSON
    )

instance PersistField Json where
    toPersistValue = PersistText . decodeUtf8 . toStrict . encode
    fromPersistValue (PersistText text) = first T.pack $ eitherDecode $ fromStrict $ encodeUtf8 text
    fromPersistValue _ = Left "Not PersistText"

instance PersistFieldSql Json where
  sqlType _ = SqlOther "JSON"