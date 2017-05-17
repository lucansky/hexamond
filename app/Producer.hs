module Main where

import Network.Socket as N (Family(AF_CAN), Socket, SockAddr(..), ProtocolNumber, SocketType(Raw), bind, socket, close, recvBufFrom)
import Network.Socket.ByteString (recv, send)
import qualified Data.ByteString as S
import Network.BSD (ifNameToIndex)
import Data.Maybe (fromJust)
import Control.Monad (forever, liftM)
import Foreign.C.Types (CUInt, CUChar)
import Foreign.Storable
import qualified Data.ByteString.Char8 as C
import Foreign.Marshal.Alloc(alloca)
import Can
import Foreign.Ptr
import Control.Concurrent

bufSize :: Int
bufSize = 8

main :: IO ()
main = do
  putStrLn "Initializing..."
  s <- initCan "vcan0"
  putStrLn "Initialized."
  canSend s
  forever $ do
    --cf <- canRead s
    --print $ show cf
    print "Sending msg..."
    canSend s
    threadDelay (10^6 * 1)

  close s
  return ()


-- receive CAN frames from every enabled CAN interface
can0 :: SockAddr
can0 = SockAddrCan 0

-- particular protocol of the protocol family PF_CAN - CAN_RAW
canRaw :: ProtocolNumber
canRaw = read "1" :: ProtocolNumber

initCan :: String ->  IO Socket
initCan addr = do
  s <- socket AF_CAN Raw canRaw
  idx <-  fromJust <$> ifNameToIndex addr
  bind s (SockAddrCan $ fromIntegral idx)
  return s
              where idx = fromJust <$> ifNameToIndex addr

canTestMsg = S.pack [ 32,32,32,0 -- can ID = 0
                    , 8,0,0,0 -- data length counter = 2 (bytes)
                    , 0x45,0x67,0x89,01 -- SYNC with some random extra bytes
                    , 01, 00, 00, 34
                    ]

canSend :: Socket -> IO Int
canSend s = send s canTestMsg

canRead :: Socket -> IO (Either String CanFrame)
canRead s =
  alloca $ \ptrCf -> do
    (cnt, _) <- recvBufFrom s (ptrCf :: Ptr CanFrame) 16
    if cnt > 0 then do
      frame <- peek ptrCf
      return $ Right frame
    else return $ Left "Buffer is empty"
