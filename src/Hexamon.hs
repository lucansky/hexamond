{-# LANGUAGE OverloadedStrings #-}

module Hexamon where

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

import Numeric (showHex)

import qualified Control.Exception as E
import Network.Wreq hiding (Raw)
import Control.Lens
import Data.Aeson.Lens (_String, key)
import Data.List (intercalate)

import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T

import Network.HTTP.Client
--import Network.HTTP.Client.Types (HttpException)

bufSize :: Int
bufSize = 8

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
                    , 4,0,0,0 -- data length counter = 2 (bytes)
                    , 0x80,123,244,55 -- SYNC with some random extra bytes
                    , 0, 0, 0, 0 -- padding
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


-- For debug purpose without real CAN interface
producerMain :: IO ()
producerMain = do
  putStrLn "Initializing..."
  s <- initCan "vcan0"
  putStrLn "Initialized."
  canSend s
  forever $ do
    --cf <- canRead s
    --print $ show cf
    putStrLn "Sending msg..."
    canSend s
    threadDelay (10^6 * 1)

  close s
  return ()

consumerMain :: IO ()
consumerMain = do
  putStrLn "Initializing..."
  s <- initCan "can0"

  putStrLn "Initialized."
  forever $ do
    cf <- canRead s
    case cf of
      Right frame -> do
	putStrLn $ show frame
        
        let msg = intercalate "" $ map (\x -> padL 2 x) $ map (\y -> showHex y "") (raw $ toMeasurement frame)
        -- let opts = defaults & param "message" .~ [T.pack $ msg] & param "serial" .~ [T.pack gatewaySerial]

        -- Change this according to your website
        let url = "http://192.168.43.76:8000/sensors/report/raw/" ++ (show $ _canFrameCanId frame)  ++ "/"++msg

        r <- safeGetUrl url 
        case r of
          Right _ -> putStrLn "OK"
          Left _ -> putStrLn "Error sending."

	return ()
      otherwise -> print $ show $ cf

  close s
  return ()

-- https://stackoverflow.com/questions/36361611/wreq-get-post-with-exception-handling
safeGetUrl :: String -> IO (Either String (Response LBS.ByteString))
safeGetUrl url = do
  let opts = defaults
  (Right <$> getWith opts url) `E.catch` handler
  where
    handler :: HttpException -> IO (Either String (Response LBS.ByteString))
    handler (HttpExceptionRequest r _) = do
      --return $ Left $ BSC.unpack (r ^. host)
      return $ Left $ "Cannot send." -- Fixme: Real reason
data Message = Message {
  raw :: [Integer]
} deriving (Show, Eq)

padL :: Int -> String -> String
padL n s
    | length s < n  = replicate (n - length s) '0' ++ s
    | otherwise = s

toMeasurement :: CanFrame -> Message
toMeasurement frame = Message (dat)
  where dat = map toInteger $ _canFrameData frame
