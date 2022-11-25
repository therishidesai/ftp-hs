{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.Binary
import qualified Control.Exception as E
import Network.Socket
import Network.Socket.ByteString.Lazy (recv, sendAll)

import GHC.Generics (Generic)


data Message = Msg { header :: String,  body :: String }
              deriving (Binary, Generic, Show)

main :: IO ()
main = runTCPClient "127.0.0.1" "3000" $ \s -> do
    let b = encode (Msg { header = "header", body = "body" })
    sendAll s b
    msg <- recv s 1024
    let b1 = decode msg :: Message
    putStrLn $ "Decode: " ++ show b1

-- from the "network-run" package.
runTCPClient :: HostName -> ServiceName -> (Socket -> IO a) -> IO a
runTCPClient host port client = withSocketsDo $ do
    addr <- resolve
    E.bracket (open addr) close client
  where
    resolve = do
        let hints = defaultHints { addrSocketType = Stream }
        head <$> getAddrInfo (Just hints) (Just host) (Just port)
    open addr = E.bracketOnError (openSocket addr) close $ \sock -> do
        connect sock $ addrAddress addr
        return sock
