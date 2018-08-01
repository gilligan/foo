module Config ( Config (..) ) where

import Db (MongoConn)

data Config = Config {
                       appPort  :: Int
                     , mongoUri :: String
                     , gracePeriodSec :: Integer
                     , dbConn :: MongoConn
                     }
