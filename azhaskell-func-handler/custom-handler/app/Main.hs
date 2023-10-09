{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Except
import Control.Monad.IO.Class

import System.Environment
import qualified Data.Text as T

import Data.Aeson

import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant.Server

import CustomHandlerIF.API

-- |
--
main :: IO ()
main = do
  putStrLn "Start AZF HCH (Azure Functions Haskell Custom Handler.)"

  port <- getEnv "FUNCTIONS_CUSTOMHANDLER_PORT"

  let portPart = if null port then ":80" else ":" ++ port
      config = Config $ "http://127.0.0.1" ++ portPart ++ "/"
      middlewares = logStdout
      server = CustomHandlerIFBackend healthCheckHandler helloHandler

  runCustomHandlerIFMiddlewareServer config middlewares server


-- |
--
healthCheckHandler :: (ExceptT ServerError IO) T.Text
healthCheckHandler = do
  liftIO $ putStrLn "healthCheckHandler called."

  return $ "health check ok. status:200"


-- |
--
helloHandler :: (ExceptT ServerError IO) Value
helloHandler = do
  liftIO $ putStrLn "helloHandler called."

  liftIO $ getEnv "FUNCTIONS_CUSTOMHANDLER_PORT" >>= putStrLn

  return $ object [ "Outputs"     .= object ["res" .= object["body" .= ("Outputs.res.body value" :: String)]],
                    "Logs"        .= (["Application Insights logs"] :: [String]),
                    "ReturnValue" .= ("ReturnValue value of function." :: String)
                  ]

