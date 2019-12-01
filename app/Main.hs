{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (MVar, newMVar, modifyMVar_, modifyMVar, readMVar)

import qualified Network.WebSockets as WS

import Lib

listenIp :: String
listenIp = "0.0.0.0"

listenPort :: Int
listenPort = 9160

main :: IO ()
main = do
    state <- newServerState
    putStrLn $ "Starting server. Listening on " ++ listenIp ++ ":" ++ (show listenPort)
    WS.runServer listenIp listenPort $ onConnect state
