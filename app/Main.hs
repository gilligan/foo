module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Api
import Config

main :: IO ()
main = do
    cfg <- loadConfig
    case cfg of
        Nothing    -> error "No configuration found. Exiting"
        (Just cfg) -> startApp cfg

startApp :: Config -> IO ()
startApp cfg =
    mkApp >>= runSettings settings
    where
        port = appPort cfg
        settings = setPort port $
            setBeforeMainLoop (hPutStrLn stderr $ "Listening on port " ++ show port)
            defaultSettings
