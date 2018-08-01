{-# LANGUAGE OverloadedStrings #-}

module Db ( 
      (=:)
    , MongoConn
    , getConnection
    , getAirports
    ) where

import Control.Monad.IO.Class
import Database.MongoDB ((=:), (!?))

import qualified Database.MongoDB as Mongo
import qualified Data.Text as T

import Models
import Data.Maybe (catMaybes)

type MongoHost = String
type MongoConn = Mongo.Pipe

airportCollection :: Mongo.Database
airportCollection = "com_holidaycheck_app_unified_booking"

getConnection :: MongoHost -> IO MongoConn
getConnection host = do
    putStr host
    Mongo.connect $ Mongo.host host 

execDB :: (MonadIO m) => Mongo.Pipe -> Mongo.Database -> Mongo.Action m a -> m a
execDB pipe = Mongo.access pipe Mongo.master

getAirports :: (MonadIO m) => Mongo.Pipe -> Mongo.Selector -> m [Airport]
getAirports pipe query = do
    doc <- execDB pipe airportCollection select
    return $ catMaybes ( toAirport <$> doc )
    where
        select = Mongo.rest =<< Mongo.find (Mongo.select query "airport")

toAirport :: Mongo.Document -> Maybe Airport
toAirport d = Airport <$> (!?) d "iataCode" <*> (!?) d "names.fallback"
