{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Config where

import Control.Concurrent (ThreadId)
import Control.Exception.Safe (throwIO)
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.IO.Class
import Control.Monad.Logger (MonadLogger(..), MonadLoggerIO(..))
import Control.Monad.Metrics (Metrics, MonadMetrics, getMetrics)
import Control.Monad.Reader (MonadReader, ReaderT, asks)
import Control.Monad.Trans.Class ( MonadTrans(lift) )
import Control.Monad.Trans.Maybe (MaybeT(..), runMaybeT)
import qualified Data.ByteString.Char8 as BS
import Database.Persist.Postgresql
       (ConnectionPool, ConnectionString, createPostgresqlPool)
import Network.Wai (Middleware)
import Network.Wai.Handler.Warp (Port)
import Network.Wai.Middleware.RequestLogger (logStdout, logStdoutDev)
import Servant (ServerError)
import System.Environment (lookupEnv)
import Katip hiding (Environment)
import Servant.Auth.Server (JWTSettings)

import Logger 

data Environment
    = Development
    | Test
    | Production
    deriving (Eq, Show, Read)

data Config
    = Config
    { configPool :: ConnectionPool
    , configEnv :: Environment
    , configMetrics :: Metrics
    , configEkgServer :: ThreadId
    , configJWTSettings :: JWTSettings
    , configLogEnv :: LogEnv
    , configPort :: Port
    }

newtype AppT m a 
    = AppT
    { runApp :: ReaderT Config (ExceptT ServerError m) a
    } deriving 
    (Functor, Applicative, Monad, MonadReader Config, MonadError ServerError
    , MonadIO
    )

type App = AppT IO

instance Monad m => MonadMetrics (AppT m) where
    getMetrics = asks configMetrics

instance MonadIO m => Katip (AppT m) where
    getLogEnv = asks configLogEnv
    localLogEnv = error "not implemented"

instance MonadIO m => MonadLogger (AppT m) where
    monadLoggerLog = adapt logMsg

instance MonadIO m => MonadLogger (KatipT m) where 
    monadLoggerLog = adapt logMsg
instance Katip IO where
    getLogEnv = defaultLogEnv
    localLogEnv _ io = io
instance MonadIO m => MonadLoggerIO (KatipT m) where 
    askLoggerIO = KatipT $ return $ adapt logMsg

setLogger :: Environment -> Middleware
setLogger Test = id
setLogger Development = logStdoutDev
setLogger Production = logStdout

katipLogger :: LogEnv -> Middleware
katipLogger env app req respond = runKatipT env $ do
    logMsg "web" InfoS "todo: received some request"
    liftIO $ app req respond

makePool :: Environment -> LogEnv -> IO ConnectionPool
makePool Test env =
    runKatipT env $ createPostgresqlPool (connStr "") (envPool Test)
makePool Development env =
    runKatipT env $ createPostgresqlPool (connStr "") (envPool Development)
makePool Production env = do
    pool <- runMaybeT $ do
        let keys = [ "host="
                   , "port="
                   , "user="
                   , "password="
                   , "dbname="
                   ]
            envs = [ "PGHOST"
                   , "PGPORT"
                   , "PGUSER"
                   , "PGPASS"
                   , "PGDATABASE"
                   ]
        envVars <- traverse (MaybeT . lookupEnv) envs
        let prodStr = BS.intercalate " " . zipWith (<>) keys $ BS.pack <$> envVars
        lift $ runKatipT env $ createPostgresqlPool prodStr (envPool Production)
    case pool of
         Nothing -> throwIO (userError "Database Configuration not present in environment.")
         Just a -> return a


envPool :: Environment -> Int
envPool Test        = 1
envPool Development = 1
envPool Production  = 8

connStr :: BS.ByteString -> ConnectionString
connStr sfx = "host=localhost dbname=testdb" <> sfx <> " user=admin password=pass port=5432"
