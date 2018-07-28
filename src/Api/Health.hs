{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module Api.Health where

import Servant
import Models

type HealthApi = "healthcheck" :> Get '[JSON] Health

healthServer :: Server HealthApi
healthServer = healthCheck

healthCheck :: Handler Health
healthCheck = return $ Health "OK"
