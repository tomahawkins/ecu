module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.Char
import Data.Word
import Text.Printf

data J1939Info = J1939Info
  { msg_id    :: Word32
  , priority  :: Word8
  , edp       :: Word8
  , dp        :: Word8
  , pf        :: Word8
  , ge        :: Word8
  , pgn       :: Word32
  , sa        :: Word8
  , da        :: Word8
  } deriving (Show, Ord, Eq)

main :: IO ()
main = do
  processCANViewLine
 
processCANViewLine :: IO ()
processCANViewLine = do
  line <- BS.getLine
  decomposeLine line
  processCANViewLine

decomposeLine :: BS.ByteString -> IO ()
decomposeLine line = do
  printTime (getTime line)
  printMsgInfo msgInfo
  --decomposePayload msgInfo (getPayload line)
  where
    msgInfo = getInfo (getID line) 
 
getInfo :: Word32 -> J1939Info
getInfo x = J1939Info
  { msg_id   = x
  , priority = getPriority x
  , edp      = getExtendedDataPage x
  , dp       = getDataPage x
  , pf       = pduFormat
  , ge       = globalExtension
  , pgn      = getParameterGroupNumber pduFormat x
  , sa       = getSourceAddress x
  , da       = getDestinationAddress pduFormat globalExtension
  }
  where
    pduFormat       = getPDUFormat x
    globalExtension = getGlobalExtension x

getPriority :: Word32 -> Word8
getPriority x = fromIntegral $ (shiftR x 26) .&. 0x7

getExtendedDataPage :: Word32 -> Word8
getExtendedDataPage x = fromIntegral $ (shiftR x 25) .&. 0x1
 
getDataPage :: Word32 -> Word8
getDataPage x = fromIntegral $ (shiftR x 24) .&. 0x1

getPDUFormat :: Word32 -> Word8
getPDUFormat x = fromIntegral $ (shiftR x 16) .&. 0xFF

getGlobalExtension :: Word32 -> Word8
getGlobalExtension x = fromIntegral $ (shiftR x 8) .&. 0xFF

getSourceAddress :: Word32 -> Word8
getSourceAddress x = fromIntegral $ (shiftR x 8) .&. 0xFF

getParameterGroupNumber :: Word8 -> Word32 -> Word32
getParameterGroupNumber pduFormat x = if pduFormat < 240
  then (shiftR x 8) .&. 0x3FF00
  else (shiftR x 8) .&. 0x3FFFF
 
getDestinationAddress :: Word8 -> Word8 -> Word8
getDestinationAddress pduFormat globalExt = if pduFormat < 240
  then 0xFE
  else globalExt
 
getItems :: [BS.ByteString] -> BS.ByteString -> [BS.ByteString]
getItems bss bs =  case BS.null bs of 
  True  -> bss
  False -> getItems (bss ++ [nextItem]) rest
  where
    (nextItem, rest) = BS.span (not . isSpace) noLeadingSpace
    noLeadingSpace   = BS.dropWhile isSpace bs

getTime :: BS.ByteString -> Word32
getTime bs = read (BS.unpack (items !! 0))
  where
    items = getItems [] bs
    
getID :: BS.ByteString -> Word32
getID bs = read (BS.unpack (items !! 1))
  where
    items = getItems [] bs
    
printTime :: Word32 -> IO ()
printTime time = BS.putStrLn bs
  where
    bs = BS.pack (printf "  Time:                  %9.3f" ms)
    ms = ((fromIntegral time) :: Double) / 1000.0

printMsgInfo :: J1939Info -> IO ()
printMsgInfo info  = do
  BS.putStrLn bs0
  BS.putStrLn bs1
  BS.putStrLn bs2
  BS.putStrLn bs3
  BS.putStrLn bs4
  BS.putStrLn bs5
  BS.putStrLn bs6
  BS.putStrLn bs7
  BS.putStrLn bs8
  where
    bs0 = BS.pack (printf "  Msg ID:                0x%08X" (msg_id   info))
    bs1 = BS.pack (printf "    Priority:            %d"     (priority info))
    bs2 = BS.pack (printf "    ExtendedDataPage:    %d"     (edp      info))
    bs3 = BS.pack (printf "    DataPage:            %d"     (dp       info))
    bs4 = BS.pack (printf "    PDU Format:          %d"     (pf       info))
    bs5 = BS.pack (printf "    Global Extension:    %d"     (ge       info))
    bs6 = BS.pack (printf "    PGN:                 0x%06X" (pgn      info))
    bs7 = BS.pack (printf "    Source Address:      %d"     (sa       info))
    bs8 = BS.pack (printf "    Destination Address: %d"     (sa       info))

