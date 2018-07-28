{-# LANGUAGE OverloadedStrings #-}

module AirportSpec where

import qualified Data.Text as T

import Servant
import Test.Hspec

import Api
import Api.Airport
import Models

testAirports :: [Airport]
testAirports = [ Airport "HAM" "Hamburg"
               , Airport "BER" "Berlin"
               , Airport "ZRH" "Zurich"
               , Airport "MUC" "Munich"
               ]

shouldSucceedWith stmt res = runHandler stmt `shouldReturn` Right res

spec :: Spec
spec = do

    describe "listAirports" $ do
        it "returns all airports without filter" $
            listAirports Nothing `shouldSucceedWith` testAirports

        it "returns the airport idenfitied by filter" $
            listAirports (Just $ T.pack "HAM") `shouldSucceedWith` [head testAirports]

        it "returns nothing if iata code does not exist" $
            listAirports (Just $ T.pack "___") `shouldSucceedWith` []

    describe "healthCheck" $ 
        it "returns OK data" $
            healthCheck `shouldSucceedWith` Health (T.pack "OK")
