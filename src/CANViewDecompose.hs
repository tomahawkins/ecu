module Main (main) where

import qualified Data.ByteString.Char8 as BS
import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Text.Printf
import System.Environment

import CANData
import CANDB
import J1939Data

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
  args <- getArgs
  case args of
    ["-h"] -> help
    ["--help"] -> help
    [] -> processCANViewLine
    _ -> help
 
help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  decomp - decompose J1939 messages from 'canview'"
  , ""
  , "EXAMPLE"
  , "  canview | decomp"
  , "  OR"
  , "  decomp < canview.log"
  , ""
  ]

processCANViewLine :: IO ()
processCANViewLine = do
  line <- BS.getLine
  decomposeLine line
  processCANViewLine

decomposeLine :: BS.ByteString -> IO ()
decomposeLine line = do
  BS.putStrLn line
  printTime (getTime line)
  printMsgInfo msgInfo
  decomposePayload msgInfo (getPayload line)
  BS.putStrLn (BS.pack "")
  where
    msgInfo = getInfo (getID line) 

decomposePayload :: J1939Info -> [Word8] -> IO ()
--decomposePayload _ payload = printBytes payload
decomposePayload info payload = case (getCANMsg info) of
  Nothing  -> BS.putStrLn bs0
    where
      bs0 = BS.pack $ concat
        [ "<Unknown>:"
        , concatMap formatHexBytes payload
        ]
  Just thisMsg -> do
    BS.putStrLn bs0
    mapM_ (decomposeSignal payload) (canMsgSignals thisMsg)
    where
      bs0 = BS.pack $ concat
        [ printf "%s:" messageName
        , concatMap formatHexBytes payload
        ]
      messageName      = canMsgName thisMsg

formatHexBytes :: Word8 -> String
formatHexBytes x = printf " %02X" x

decomposeSignal :: [Word8] -> CANSignal -> IO ()
decomposeSignal payload thisSignal = BS.putStrLn bs0
  where
    bs0       = BS.pack (printf ("    %s: " ++ numberFmt) name signal)
    name      = canSignalName thisSignal
    signal    = decodeSignal payload thisSignal
    numberFmt = case (canSignalFactor thisSignal) of
      1.0 -> "%.0f"
      _   -> "%f"
 
decodeSignal :: [Word8] -> CANSignal -> Float
decodeSignal payload signal = (fromIntegral binaryValue) * factor + offset
  where
    binaryValue = case (canSignalEndian signal) of
      CANBigEndian    -> getBinaryValueBE word64 signal
      CANLittleEndian -> getBinaryValueLE word64 signal
    factor = canSignalFactor signal
    offset = canSignalOffset signal
    word64 = toWord64 payload

getBinaryValueLE :: Word64 -> CANSignal -> Word64
getBinaryValueLE word64 signal = tmp2 .&. mask
  where
    tmp          = shiftR word64 ((7 - endByte) * 8)
    tmp1         = rearrangeBytesLEtoBE tmp numBytes
    tmp2         = shiftR tmp1 (mod startBit 8)
    mask         = (shiftL 1 bitLength) - 1
    numBytes     = 1 + (endByte - startByte)
    startByte    = div startBit 8
    endByte      = div (startBit + bitLength -1) 8
    bitLength    = fromIntegral $ canSignalBitLength signal
    startBit     = fromIntegral $ canSignalStartBit signal

getBinaryValueBE :: Word64 -> CANSignal -> Word64
getBinaryValueBE word64 signal = shiftedValue .&. mask
  where
    shiftedValue = shiftR word64 (getShiftAmount signal)
    mask         = (shiftL 1 bitLength) - 1
    bitLength    = fromIntegral $ canSignalBitLength signal

getShiftAmount :: CANSignal -> Int
getShiftAmount signal = 64 - bitLength - ((div startBit 8) * 8 + (7 - (mod startBit 8)))
  where
    bitLength = fromIntegral $ canSignalBitLength signal
    startBit  = fromIntegral $ canSignalStartBit signal
                        
rearrangeBytesLEtoBE :: Word64 -> Int -> Word64
rearrangeBytesLEtoBE word64 numBytes = toWord64 rearranged
  where
     desiredBytes =  drop (8 - numBytes) (fromWord64 word64)
     numDontCare = 8 - numBytes
     rearranged = (replicate numDontCare 0xFF) ++ (reverse desiredBytes)

getCANMsg :: J1939Info -> Maybe CANMsg
getCANMsg info = find (isSamePGN (msg_id info)) (canDbMsgs canDB)

isSamePGN :: Word32 -> CANMsg -> Bool
isSamePGN  matchID thisMsg = matchPGN == thisPGN
  where
    thisPGN        = getParameterGroupNumber thisID
    matchPGN       = getParameterGroupNumber matchID
    thisID         = canMsgId thisMsg

--printBytes :: [Word8] -> IO ()
--printBytes payload  = do 
--  BS.putStrLn bs0
--  mapM_ printByte (zip [ 1 .. ] payload)
--  where
--    bs0 = BS.pack (printf "  Payload:")
--
--printByte :: (Int, Word8) -> IO ()
--printByte (index, byte)  = BS.putStrLn bs0
--  where
--    bs0 = BS.pack (printf "    Byte%d: %02X" index byte)

getInfo :: Word32 -> J1939Info
getInfo x = J1939Info
  { msg_id   = x
  , priority = getPriority x
  , edp      = getExtendedDataPage x
  , dp       = getDataPage x
  , pf       = pduFormat
  , ge       = globalExtension
  , pgn      = getParameterGroupNumber x
  , sa       = getSourceAddress x
  , da       = getDestinationAddress x
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
getSourceAddress x = fromIntegral $ x .&. 0xFF

getParameterGroupNumber :: Word32 -> Word32
getParameterGroupNumber x = if (getPDUFormat x) < 240
  then (shiftR x 8) .&. 0x3FF00
  else (shiftR x 8) .&. 0x3FFFF
 
getDestinationAddress :: Word32 -> Word8
getDestinationAddress x = if (getPDUFormat x) < 240
  then (getGlobalExtension x)
  else 0xFE
 
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

getPayload :: BS.ByteString -> [Word8]
getPayload bs = map read (map BS.unpack hexBytes)
  where
    hexBytes = map (BS.append searchPrefix) (hexPairs hexBS)
    hexBS = case (BS.isPrefixOf searchPrefix payloadBS) of
      True -> BS.drop 2 payloadBS
      False -> payloadBS
    searchPrefix = BS.pack "0x"
    payloadBS = items !! 2
    items = getItems [] bs

hexPairs :: BS.ByteString -> [BS.ByteString]
hexPairs bs
  | BS.null bs = []
  | otherwise = BS.take 2 bs : hexPairs (BS.drop 2 bs)

printTime :: Word32 -> IO ()
printTime time = BS.putStrLn bs
  where
    bs = BS.pack (printf "Time:                  %9.3f" ms)
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
    bs0 = BS.pack (printf "Msg ID:                0x%08X"  (msg_id   info))
    bs1 = BS.pack (printf "  Priority:            %d"      (priority info))
    bs2 = BS.pack (printf "  ExtendedDataPage:    %d"      (edp      info))
    bs3 = BS.pack (printf "  DataPage:            %d"      (dp       info))
    bs4 = BS.pack (printf "  PDU Format:          %d"      (pf       info))
    bs5 = BS.pack (printf "  Global Extension:    %d"      (ge       info))
    bs6 = BS.pack (printf "  PGN:                 0x%06X"  (pgn      info))
    bs7 = BS.pack (printf "  Source Address:      %d (%s)" (sa       info) saName)
    bs8 = BS.pack (printf "  Destination Address: %d (%s)" (da       info) daName)
    saName = case (lookup (sa info) sourceAddressTable) of
      Nothing -> error (printf "couldn't find name for source address 0x%02x" (sa info))
      Just name -> name
    daName = case (lookup (da info) sourceAddressTable) of
      Nothing -> error (printf "couldn't find name for destination address 0x%02x" (da info))
      Just name -> name

