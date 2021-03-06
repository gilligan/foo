{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Init (startApp) where

import Network.Wai.Handler.Warp    (defaultSettings, runSettings, setBeforeMainLoop, setPort, setLogger)
import Safe                        (readMay)
import System.Exit                 (exitFailure)
import System.IO                   (hPutStrLn, stderr)
import System.Posix.Env            (getEnv)
import Control.Monad.Except
import Network.Wai.Logger          (withStdoutLogger)
import Data.Pool

import Api   (mkApp)
import Db    (getConnection', closeConnection)
import Types (AppCtx(..), AppConfig(..), InitError(..))

startApp :: IO ()
startApp = initConfig >>= \case
    Right c -> runWithConfig c
    Left  err -> putStrLn (reportError err) >> exitFailure

reportError :: InitError -> String
reportError ConfigErrorPort = "Error: service port not configured. Please set the APP_PORT environment variable"
reportError ConfigErrorHost = "Error: mongo host is not configured. Please set the MONGO_URI environment variable"
reportError InitErrorDbConnection = "Error: failed to connect to mongo database"
reportError ConfigErrorGraceSecs = "Error: grace shutdown timeout not configured. Please set the GRACE_PERIOD_SEC environment variable"
    
runWithConfig :: AppConfig -> IO ()
runWithConfig config = do
    dbPool <- createPool (getConnection' (mongoUri config)) closeConnection 25 2000 10

    withStdoutLogger $ \logger -> do
        let settings = setPort (appPort config) 
                    $ setBeforeMainLoop (hPutStrLn stderr $ "Starting up service on port " ++ show (appPort config))
                    $ setLogger logger defaultSettings

        runSettings settings (mkApp $ AppCtx config dbPool)


maybeToEither :: a -> Maybe b -> Either a b
maybeToEither = flip maybe Right . Left

getEnvWith :: (String -> Maybe b) -> a -> String -> ExceptT a IO b
getEnvWith readF err str =  liftIO (getEnv str) >>= \case
  Just val -> liftEither $ maybeToEither err (readF val)
  Nothing  -> throwError err

readEnv :: Read b => a -> String -> ExceptT a IO b
readEnv = getEnvWith readMay

readEnvStr :: a -> String -> ExceptT a IO String
readEnvStr = getEnvWith Just

initConfig :: IO (Either InitError AppConfig)
initConfig = runExceptT $ do
    port      <- readEnv ConfigErrorPort "APP_PORT"
    mongoHost <- readEnvStr ConfigErrorHost "MONGO_URI"
    graceSecs <- readEnv ConfigErrorGraceSecs "GRACE_PERIOD_SEC"

    return $ AppConfig port mongoHost graceSecs
