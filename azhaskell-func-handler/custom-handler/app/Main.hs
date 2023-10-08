{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}

module Main where

import Control.Monad.Except
import Control.Monad.IO.Class

import qualified Data.Text as T

import Network.Wai.Middleware.RequestLogger (logStdout)
import Servant.Server

import CustomHandlerIF.API

-- |
--
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"

  let config = Config "http://127.0.0.1/"
      middlewares = logStdout
      server = CustomHandlerIFBackend healthCheckHandler

  runCustomHandlerIFMiddlewareServer config middlewares server


-- |
--
healthCheckHandler :: (ExceptT ServerError IO) T.Text
healthCheckHandler = do
  liftIO $ putStrLn "healthCheckHandler called."

  return $ "health check ok. status:200"

