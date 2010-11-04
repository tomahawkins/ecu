module CANData
  ( Msg (..)
  , BusType (..)
  , toWord64
  , fromWord64
  , toWord32
  , fromWord32
  , CANDB (..)
  , CANMsg (..)
  , CANMsgAttr
  , CANEndian (..)
  , CANSign (..)
  , CANSignalType (..)
  , CANSignalMux (..)
  , CANSignal (..)
  , getSubId
  , getDbcMsgs
  ) where

import Data.Bits
import Data.List
import Data.Word
import Text.Printf

data Msg = Msg Word32 [Word8] deriving (Eq, Ord)

data BusType = Extended | Standard

instance Show Msg where
  show (Msg id payload) = printf "0x%08X  0x%s" id $ concatMap formatByte payload

formatByte :: Word8 -> String
formatByte b = printf "%02X" b

toWord64 :: [Word8] -> Word64
toWord64 a = foldl1 (.|.) [ shiftL (fromIntegral a) $ i * 8 | (a, i) <- zip (a ++ [0..]) $ reverse [0..7] ]

fromWord64 :: Word64 -> [Word8]
fromWord64 a = [ fromIntegral ((shiftR a $ i * 8) .&. 0xFF) | i <- reverse [0..7] ]

toWord32 :: [Word8] -> Word32
toWord32 a = foldl1 (.|.) [ shiftL (fromIntegral a) $ i * 8 | (a, i) <- zip (a ++ [0..]) $ reverse [0..3] ]

fromWord32 :: Word32 -> [Word8]
fromWord32 a = [ fromIntegral ((shiftR a $ i * 8) .&. 0xFF) | i <- reverse [0..3] ]

data CANDB = CANDB
  { canDbName :: String
  , canDbNodes :: [String]
  , canDbMsgs  :: [CANMsg]
  } deriving (Read, Show, Eq, Ord)

data CANMsg = CANMsg
  { canMsgId      :: Word32
  , canMsgName    :: String
  , canMsgDlc     :: Word8
  , canMsgTxNode  :: String
  , canMsgAttrs   :: [CANMsgAttr]
  , canMsgSignals :: [CANSignal]
  } deriving (Read, Show, Eq, Ord)

  -- CANMsgAttr = (Name,  Value )
type CANMsgAttr = (String,String)

data CANEndian = CANBigEndian | CANLittleEndian
  deriving (Read, Show, Eq, Ord)

data CANSign = CANSigned | CANUnsigned
  deriving (Read, Show, Eq, Ord)

data CANSignalType = CANSignalTypeNormal | CANSignalTypeFloat | CANSignalTypeDouble
  deriving (Read, Show, Eq, Ord)

data CANSignalMux = CANSignalMuxNone | CANSignalMuxer | CANSignalMuxed Word64
  deriving (Read, Show, Eq, Ord)

data CANSignal = CANSignal
  { canSignalName      :: String
  , canSignalStartBit  :: Word8
  , canSignalBitLength :: Word8
  , canSignalEndian    :: CANEndian
  , canSignalSign      :: CANSign
  , canSignalFactor    :: Float
  , canSignalOffset    :: Float
  , canSignalMin       :: Float
  , canSignalMax       :: Float
  , canSignalUnit      :: String
  , canSignalRxNodes   :: [String]
  , canSignalType      :: CANSignalType
  , canSignalMux       :: CANSignalMux
  } deriving (Read, Show, Eq, Ord)

getSubId :: CANMsg -> String
getSubId msg = printf "%04X" ((canMsgId msg) .&. 0xFFFF)
  
getDbcMsgs :: CANDB -> String -> [CANMsg]
getDbcMsgs candb prefix = filter (\ x -> isPrefixOf prefix (canMsgName x)) (canDbMsgs candb) 

