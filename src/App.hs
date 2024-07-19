{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module App where

import Control.Monad.IO.Class
import Control.Monad.Except (ExceptT, MonadError)
import Control.Monad.Reader (MonadReader, ReaderT, asks) 
import Data.Pool
import Database.PostgreSQL.Simple (Connection)
import Servant (ServerError)
import Data.Text


-- Log Levels
data LogLevel = Debug | Info | Warn | Error deriving (Show, Eq, Ord)

logInfo :: MonadIO m => Text -> AppT m ()
logInfo msg = do
    level <- asks logLevel
    if (level <= Info) then liftIO $ putStrLn $ "INFO: " ++ (unpack msg) else pure ()
        
data Env
  = Env
      { configConnection :: Pool Connection,
        huggingfaceToken :: String,
        logLevel :: LogLevel
      }

newtype AppT m a
  = AppT
      { runAppT :: ReaderT Env (ExceptT ServerError m) a
      }
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadIO, MonadError ServerError)
