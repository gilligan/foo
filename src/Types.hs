{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Types where

import Servant.Server

import Control.Monad.Reader
import Control.Monad.Except

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Except

import Config

newtype AppT m a = AppT { runApp :: ReaderT Config (ExceptT ServantErr m) a } 
    deriving (Functor, Applicative, Monad, MonadReader Config, MonadError ServantErr , MonadIO )

type App = AppT IO
