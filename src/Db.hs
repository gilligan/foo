{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Db ( 
      (=:)
    , MongoConn
    , getConnection
    , getAirports
    , getTimezones
    , getEntity
    ) where

import           Control.Exception (IOException, catch)
import           Control.Monad.IO.Class
import           Control.Monad.Reader (asks)
import           Database.MongoDB ((=:), (!?))
import           Data.Maybe (mapMaybe)
import qualified Database.MongoDB as Mongo

import Models
import Types

class Entity a where
  collection :: Mongo.Database
  format :: Mongo.Document -> Maybe a

instance Entity Airport where
    collection = "airport"
    format = toAirport

instance Entity Timezone where
    collection = "timezone"
    format = toTimezone

-- | Retrieve an Entity from the database via the provided mongo query
getEntity :: forall m res. (MonadIO m, Entity res) => Mongo.Selector -> AppT m [res]
getEntity query = do
    conn <- asks dbConn
    doc <- liftIO $ execDB conn airportDatabase select
    return $ mapMaybe format doc
        where
            select = Mongo.rest =<< Mongo.find (Mongo.select query (collection @res))
            execDB pipe = Mongo.access pipe Mongo.master
            airportDatabase = "com_holidaycheck_app_unified_booking"

getConnection :: MongoHost -> IO (Maybe Mongo.Pipe)
getConnection h = (Just <$> Mongo.connect (Mongo.host h)) `catch` errorHandler 
    where
        errorHandler (_ :: IOException) = putStrLn errorMsg >> return Nothing
        errorMsg = "Failed to establish a connection to mongodb: " <> h

getAirports :: MonadIO m => Mongo.Selector -> AppT m [Airport]
getAirports = getEntity

getTimezones :: MonadIO m => Mongo.Selector -> AppT m [Timezone]
getTimezones = getEntity

toAirport :: Mongo.Document -> Maybe Airport
toAirport d = Airport <$> (!?) d "iataCode" <*> (!?) d "names.fallback"

toTimezone :: Mongo.Document -> Maybe Timezone
toTimezone d = Timezone <$> (!?) d "iataCode" <*> (!?) d "timezone"
