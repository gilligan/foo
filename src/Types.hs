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

type MongoHost = String
type MongoConn = Mongo.Pipe
type App       = AppT IO

data Config = Config {
                       appPort  :: Int
                     , mongoUri :: String
                     , gracePeriodSec :: Integer
                     , dbConn :: MongoConn
                     }

newtype AppT m a = AppT { runApp :: ReaderT Config (ExceptT ServantErr m) a } 
    deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr , MonadIO )

