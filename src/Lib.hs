{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( onConnect
    , newServerState
    ) where

import Control.Concurrent ( MVar
                          , newMVar
                          , modifyMVar_
                          , modifyMVar
                          , readMVar
                          )
import Control.Exception ( try )
import Control.Monad ( forM_, forever )
import Data.GUID ( genText )
import Data.Text ( Text )

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Network.WebSockets as WS

type Client = (Text, WS.Connection)

type ServerState = MVar [Client]

newServerState :: IO ServerState
newServerState = newMVar []

addClient' :: Client -> [Client] -> [Client]
addClient' client clients = client : clients

addClient :: Client -> ServerState -> IO ()
addClient client state = modifyMVar_ state $ \clients -> do
    let cs' = addClient' client clients
    putStrLn "Current clients"
    print (map fst cs')
    return cs'

removeClient' :: Client -> [Client] -> [Client]
removeClient' client = filter ((/= fst client) . fst)

removeClient :: Client -> ServerState -> IO ()
removeClient client state = modifyMVar_ state $ \clients -> do
    let cs' = removeClient' client clients
    putStrLn "Remaining clients"
    print (map fst cs')
    return cs'

broadcast' :: Text -> [Client] -> IO ()
broadcast' message clients = do
    T.putStrLn message
    forM_ clients $ \(_, conn) -> WS.sendTextData conn message

broadcast :: Text -> ServerState -> IO ()
broadcast message state = do
    readMVar state >>= broadcast' message

onConnect :: ServerState -> WS.PendingConnection -> IO ()
onConnect state pending = do
    putStrLn "onConnect"
    conn <- WS.acceptRequest pending
    connId <- genText
    WS.sendTextData conn ("Welcome " `mappend` connId)
    WS.forkPingThread conn 30
    addClient (connId, conn) state
    broadcast (connId `mappend` " joined") state
    talk (connId, conn) state

onDisconnect :: Client -> ServerState -> IO ()
onDisconnect (connId, conn) state = do
    putStrLn "onDisconnect"
    removeClient (connId, conn) state
    broadcast (connId `mappend` " left") state

talk :: Client -> ServerState -> IO ()
talk (connId, conn) state = do
    msgE <- try (WS.receiveData conn)
    case (msgE :: Either WS.ConnectionException Text) of
        Left e -> do
            onDisconnect (connId, conn) state
        Right msg -> do
            onMessage (connId, conn) state msg
            talk (connId, conn) state

onMessage :: Client -> ServerState -> Text -> IO ()
onMessage (connId, conn) state msg = do
    putStrLn "onMessage"
    broadcast (connId `mappend` ": " `mappend` msg) state