module Logger
    ( adapt
    , defaultLogEnv
    , logMsg
    , runKatipT
    , KatipT (..)
    , Katip (..)
    , LogEnv
    , Severity
    ) where

import Control.Monad.Logger
import qualified Control.Monad.Logger as Logger
import Katip
import qualified System.IO as IO
import qualified System.Log.FastLogger as FastLogger

defaultLogEnv :: IO LogEnv
defaultLogEnv = do
    handleScribe <- mkHandleScribe ColorIfTerminal IO.stdout (permitItem DebugS) V2
    env <- initLogEnv "servant-persistent" "production"
    registerScribe "stdout" handleScribe defaultScribeSettings env

fromLevel :: LogLevel -> Severity
fromLevel LevelDebug = DebugS
fromLevel LevelInfo = InfoS
fromLevel LevelWarn = WarningS
fromLevel LevelError = ErrorS
fromLevel (LevelOther _) = NoticeS

adapt :: (ToLogStr msg, Applicative m, Katip m) =>
         (Namespace -> Severity -> Katip.LogStr -> m ()) ->
         Loc -> LogSource -> LogLevel -> msg -> m ()
adapt f _ src lvl msg =
    f ns (fromLevel lvl) $ logStr' msg
  where
    ns = Namespace [src]
    logStr' = Katip.logStr . FastLogger.fromLogStr . Logger.toLogStr
