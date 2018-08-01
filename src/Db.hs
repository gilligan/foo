{-# LANGUAGE OverloadedStrings #-}

module Db ( 
      (=:)
    , MongoConn
    , getConnection
    , getAirports
    ) where

import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Database.MongoDB ((=:), (!?))
import           Data.Maybe (catMaybes)
import qualified Database.MongoDB as Mongo

import Models
import Types


airportCollection :: Mongo.Database
airportCollection = "com_holidaycheck_app_unified_booking"

getConnection :: MongoHost -> IO MongoConn
getConnection host = do
    putStr host
    Mongo.connect $ Mongo.host host 

execDB :: (MonadIO m) => Mongo.Pipe -> Mongo.Database -> Mongo.Action m a -> m a
execDB pipe = Mongo.access pipe Mongo.master

getAirports :: MonadIO m => Mongo.Selector -> AppT m [Airport]
getAirports query = do
    conn <- asks dbConn
    doc <- liftIO $ execDB conn airportCollection select
    return $ catMaybes ( toAirport <$> doc)
        where
        select = Mongo.rest =<< Mongo.find (Mongo.select query "airport")

toAirport :: Mongo.Document -> Maybe Airport
toAirport d = Airport <$> (!?) d "iataCode" <*> (!?) d "names.fallback"
