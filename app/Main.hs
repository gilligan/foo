module Main where

import Network.Wai
import Network.Wai.Handler.Warp
import Servant
import System.IO

import Api

main :: IO ()
main = runSettings settings =<< mkApp
    where
        settings = setPort 3000 $
            setBeforeMainLoop (hPutStrLn stderr "Listening on port 3000")
            defaultSettings
