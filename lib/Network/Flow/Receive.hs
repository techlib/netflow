{-|
Module      :  Network.Flow.Receive
Description :  Receiving Network Flows over UDP
Copyright   :  (c) Jan Dvořák
License     :  MIT

Maintainer  :  mordae@mordae.eu
Stability   :  unstable
Portability :  non-portable (ghc)
-}

{-# LANGUAGE NoImplicitPrelude #-}

module Network.Flow.Receive
( datagrams
, withReceiver
) where
  import BasePrelude hiding (yield, length)

  import Data.ByteString (ByteString, length)

  import Network.Socket hiding (recvFrom)
  import Network.Socket.ByteString
  import Pipes


  -- |
  -- Produce a stream of datagrams.
  --
  datagrams :: Socket -> Producer (ByteString, SockAddr) IO ()
  datagrams sock = do
    forever $ do
      (buf, addr) <- lift $ recvFrom sock 9000

      when (length buf > 0) $ do
        yield (buf, addr)


  -- |
  -- Perform an action with socket listening for incoming datagrams.
  --
  withReceiver :: String -> String -> (Socket -> IO r) -> IO r
  withReceiver host port body = withSocketsDo $ do
    -- Obtain an address information matching both host and port name.
    -- TODO: Try one after another instead of just the first.
    (addr:_) <- getAddrInfo Nothing (Just host) (Just port)

    -- Run the body and then close the socket.
    bracket (udpReceiver addr) sClose body


  -- |
  -- Open and bind an UDP socket.
  --
  udpReceiver :: AddrInfo -> IO Socket
  udpReceiver addr = do
    -- Attempt to construct the socket.
    sock <- socket (addrFamily addr) Datagram defaultProtocol

    -- Bind socket to a local address.
    bind sock (addrAddress addr)

    -- Return the ready socket.
    return sock


-- vim:set ft=haskell sw=2 ts=2 et:
