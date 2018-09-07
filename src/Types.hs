{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Types where

import           Control.Monad.Except
import           Control.Monad.Reader
import           Control.Monad.Trans.Class  ()
import           Control.Monad.Trans.Except ()
import           Control.Monad.Trans.Reader ()
import qualified Database.MongoDB as Mongo
import           Servant.Server
import           Data.Pool

type MongoHost = String
type MongoConn = Mongo.Pipe
type App       = AppT IO

data InitError = ConfigErrorPort
               | ConfigErrorHost
               | ConfigErrorGraceSecs
               | InitErrorDbConnection
               deriving (Show, Eq)

data AppConfig = AppConfig { appPort  :: Int
                           , mongoUri :: String
                           , gracePeriodSec :: Integer
                           }

data AppCtx = AppCtx { cfg  :: AppConfig
                     , pool :: Pool MongoConn
                     }

newtype AppT m a = AppT { runApp :: ReaderT AppCtx (ExceptT ServantErr m) a } 
    deriving (Functor, Applicative, Monad, MonadReader AppCtx, MonadError ServantErr , MonadIO )

