module CAN
  ( module CANData
  , Bus
  , initCAN
  , openBus
  , closeBus
  , recvMsg
  , recvMsgWait
  , sendMsg
  , busTime
  , busTime'
  , testBusStatus
  , testIFStatus
  , sendUntil
  , toPayload
  , fromPayload
  , flushRxQueue
  ) where

import Data.Bits
import Data.Char
import Data.Word

import CANData

foreign import ccall unsafe initCAN         :: IO ()
foreign import ccall unsafe canOpenExt_     :: Int -> IO Int
foreign import ccall unsafe canOpenStd_     :: Int -> IO Int
--foreign import ccall unsafe canOpenVirtual_ :: Int -> IO Int
foreign import ccall unsafe canClose_       :: Int -> IO ()
--foreign import ccall unsafe canRead_        :: Int -> IO Int
foreign import ccall unsafe canReadWait_    :: Int -> Int -> IO Int
foreign import ccall unsafe canReadId_      :: IO Int
foreign import ccall unsafe canReadTime_    :: IO Int
foreign import ccall unsafe canReadMsgLen_  :: IO Int
foreign import ccall unsafe canReadMsg_     :: Int -> IO Char
foreign import ccall unsafe canWriteByte_   :: Char -> Int -> IO ()
foreign import ccall unsafe canWrite_       :: Int -> Int -> Int -> IO ()
foreign import ccall unsafe canReadTimer_   :: Int -> IO Int
foreign import ccall unsafe canFlushReceiveQueue_ :: Int -> IO Int

newtype Bus = Bus Int

openBus :: Int -> BusType -> IO Bus
openBus channel busType = do
  h <- case busType of
    Standard -> canOpenStd_ channel
    Extended -> canOpenExt_ channel
  return $ Bus h

closeBus :: Bus -> IO ()
closeBus (Bus h) = canClose_ h

-- | Receive a message if a message is available.  Includes receive time (ms).  
recvMsg :: Bus -> IO (Maybe (Int, Msg))
recvMsg b = recvMsgWait b 0

-- | Waits a number of ms to receive a message.  Includes receive time (ms).  
recvMsgWait :: Bus -> Int -> IO (Maybe (Int, Msg))
recvMsgWait (Bus h) wait = do
  status <- canReadWait_ h wait
  id <- canReadId_
  if status /= 0 || id == 0 then return Nothing else do
    len  <- canReadMsgLen_
    msg  <- mapM canReadMsg_ [0 .. len - 1]
    t    <- canReadTime_
    return $ Just (t, Msg (fromIntegral id) (map (fromIntegral . ord) msg))

-- | Send a message.
sendMsg :: Bus -> Msg -> IO ()
sendMsg (Bus h) (Msg id payload') = do
  mapM_ (\ (b, i) -> canWriteByte_ (chr $ fromIntegral b) i) $ zip payload [0..]
  canWrite_ h (fromIntegral id) $ length payload
  where
  payload = take 8 payload'

flushRxQueue :: Bus -> IO ()
flushRxQueue (Bus h) = do
  canFlushReceiveQueue_ h
  return ()

-- | Current driver time (ms).
busTime :: Bus -> IO (Maybe Int)
busTime (Bus h) = do
  status <- canReadTimer_ h
  if status /= 0 then return Nothing else do
    t <- canReadTime_
    return $ Just t

busTime' :: Bus -> IO Int
busTime' bus = do
    t <- busTime bus
    case t of
      Nothing -> busTime' bus
      Just t  -> return t

testBusStatus :: Bus -> IO Int
testBusStatus (Bus h) = do
  status <- canReadWait_ h 500
  return status

testIFStatus :: Bus -> IO Int
testIFStatus (Bus h) = do
  status <- canReadTimer_ h
  return status

-- | Send a message periodically (in ms) until a certain message is received.
sendUntil :: Bus -> Int -> Msg -> (Msg -> Bool) -> IO Msg
sendUntil bus retryTime msg isAck = getStartTime >>= f1
  where
  f1 startTime = sendMsg bus msg >> f2 startTime startTime
  f2 startTime currentTime | currentTime - startTime > retryTime = f1 currentTime
  f2 startTime currentTime = do
    m <- recvMsgWait bus $ max 0 $ retryTime - (currentTime - startTime)
    case m of
      Nothing                 -> f1 currentTime
      Just (t, m) | isAck m   -> return m
                  | otherwise -> f2 startTime t

  getStartTime = do
    m <- recvMsg bus
    case m of
      Nothing -> getStartTime
      Just (t, _) -> return t

toPayload :: Word64 -> [Word8]
toPayload payload = [ fromIntegral (shiftR payload (b * 8) .&. 0xFF) | b <- reverse [0..7] ]

fromPayload :: [Word8] -> Word64
fromPayload payload = foldr1 (.|.) [ shiftL (fromIntegral x .&. 0xFF) (b * 8) | (x, b) <- zip payload (reverse [0..7]) ]

