{-# LANGUAGE BangPatterns, OverloadedStrings #-}

module Init where

import Config (Config(..), Environment(..), makePool, katipLogger, setLogger)

import Say 
import System.Environment (lookupEnv)
import Safe (readMay)
import Data.Text (Text)
import Control.Concurrent (killThread)
import qualified Data.Text as Text
import qualified Katip
import qualified Control.Monad.Metrics as M
import Network.Wai.Metrics (metrics, registerWaiMetrics)
import qualified Data.Pool as Pool
import Network.Wai (Application)
import Database.Persist.Postgresql (runSqlPool)
import Database (doMigrations)
import Data.Typeable (typeOf)
import Lens.Micro ((^.))

import App (mkApp)

import Data.Monoid ()
import Control.Exception.Safe
import System.Remote.Monitoring (forkServer, serverMetricStore, serverThreadId)
import Logger (defaultLogEnv)
import Servant.Auth.Server (defaultJWTSettings, fromSecret)
import Network.Wai.Handler.Warp (run)

lookupSetting :: Read a => String -> a -> IO a
lookupSetting env def = do
    maybeValue <- lookupEnv env
    case maybeValue of
        Nothing -> 
            return def
        Just str ->
            maybe (handleFailedRead str) return (readMay str)
  where
    handleFailedRead str = 
        error $ mconcat
            [ "Failed to read [["
            , str
            , "]] for environment variable"
            , env
            ]

tshow :: Show a => a -> Text
tshow = Text.pack . show

withConfig :: (Config -> IO a) -> IO a
withConfig runAppFromConfig = do
    say "Init.withConfig"
    port <- lookupSetting "PORT" 8081
    say $ "port: " <> tshow port
    env <- lookupSetting "ENV" Development
    say $ "env: " <> tshow env
    secret <- lookupSetting "SECRET" "!#$ASDFeqwrqwerjlkq$%%#%aioqpKAOQKqoou!#411234asdfOQWP431$$%&&LKASDOWEQRJASDFNXZCOISADJQOWER$$%%&!" -- WARNING: This is just an example, in the real world you have to set the env variable
    let jwk = fromSecret secret
    bracket defaultLogEnv (\x -> say "Closing katip scribes" >> Katip.closeScribes x) $ \logEnv -> do
        say $ "Got log env"
        !pool <- makePool env logEnv `onException` say "Exception while running makePool"
        say "Got pool"
        bracket (forkServer "localhost" 8082) (\x -> say "Closing ekg" >> do killThread $ serverThreadId x) $ \ekgServer -> do
            say "Forked ekg server"
            let store = serverMetricStore ekgServer
            say "RegisteredWaiMetrics"
            metr <- M.initializeWith store
            say "Got metrics"
            runAppFromConfig Config 
                { configPool = pool
                , configEnv = env
                , configMetrics = metr
                , configLogEnv = logEnv
                , configPort = port
                , configJWTSettings = defaultJWTSettings jwk
                , configEkgServer = serverThreadId ekgServer
                }

initialize :: Config -> IO Application
initialize cfg = do
    say "Init.initialize"
    waiMetrics <- registerWaiMetrics (configMetrics cfg ^. M.metricsStore)
    say "Wai metrics registered"
    let kLogger = katipLogger (configLogEnv cfg)
    say "Katip logger was set"
    let logger = setLogger (configEnv cfg)
    say "Logger was set"
    bracket 
        (say "Starting to run migrations")
        (\_ -> say "Migrations complete")
        $ \_ -> do
            say "Running migrations..."
            runSqlPool doMigrations (configPool cfg) `catch` \(SomeException e) -> do
                say $ mconcat
                    [ "exception in doMigrations of type:"
                    , tshow (typeOf e)
                    , ", shown:"
                    , tshow e
                    ]
                throwIO e
            say "Completed runSqlPool"
    pure . kLogger. logger . metrics waiMetrics . mkApp $ cfg

shutdownApp :: Config -> IO ()
shutdownApp cfg = do
    _ <- Katip.closeScribes (configLogEnv cfg)
    Pool.destroyAllResources (configPool cfg)

    killThread (configEkgServer cfg)
    pure ()

runApp :: IO ()
runApp = do
    say "Init.runApp"
    withConfig $ \config -> do
        say "Acquired config"
        app <- initialize config
            `finally` say "Initialized config"
        say "Running app with set configuration"
        run (configPort config) app
            `finally` say "Server is closed"
