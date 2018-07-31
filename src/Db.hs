{-# LANGUAGE OverloadedStrings #-}

module Db ( 
      HostName
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

newtype HostName = HostName T.Text
    deriving (Eq, Show)

type MongoConn = Mongo.Pipe

airportCollection :: Mongo.Database
airportCollection = "com_holidaycheck_app_unified_booking"

getConnection :: HostName -> IO Mongo.Pipe
getConnection (HostName n) = Mongo.connect $ Mongo.host (T.unpack n)

execDB pipe = Mongo.access pipe Mongo.master

getAirports :: MonadIO m => Mongo.Pipe -> m [Airport]
getAirports pipe = do
    doc <- execDB pipe airportCollection select
    return $ catMaybes ( toAirport <$> doc )
    where
        select = Mongo.rest =<< Mongo.find (Mongo.select [] "airport")

getAirportByIata :: (MonadIO m, Mongo.Val p) => Mongo.Pipe -> p -> m (Maybe Airport)
getAirportByIata pipe iata = do
    doc <- execDB pipe airportCollection select
    return $ maybe Nothing toAirport doc
        where
            select = Mongo.findOne (Mongo.select ["iataCode" =: iata ] "airport")

toAirport :: Mongo.Document -> Maybe Airport
toAirport d = Airport <$> (!?) d "iataCode" <*> (!?) d "names.fallback"
