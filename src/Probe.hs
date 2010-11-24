module Main (main) where

import Control.Monad
import Data.Bits
import Data.Char
import Data.Digest.CRC32
import Data.Int
import Data.VCD hiding (Bool, Double)
import Data.Word
import System.Environment
import System.IO
import Text.Printf
import Unsafe.Coerce

import CAN

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["config", file] -> do
      initCAN
      bus <- openBus 0 Extended
      flushRxQueue bus
      saveConfig bus file
      closeBus bus
    [file] -> do
      initCAN
      bus <- openBus 0 Extended
      flushRxQueue bus
      startProbes bus file
      closeBus bus
    _ -> help

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  probe - Extracts configuration and signal probes from an ECU."
  , ""
  , "SYNOPSIS"
  , "  probe [config] <file>"
  , ""
  , "COMMANDS"
  , "  config <file>    Download probe configuration from ECU."
  , "  <file>           Given probe config file, pipe probes from ECU to VCD on stdin."
  , ""
  , "CAN IDS"
  , "  18ff00fa             Configuration upload request (sent by tool)."
  , "  18ff01ef             Configuration header (length and checksum)."
  , "  18ff02ef             Configuration data."
  , "  18ff03ef - 18ff7fef  Probe data."
  , ""
  ]

data Type
  = Bool
  | Int8
  | Int16
  | Int32
  | Int64
  | Word8
  | Word16
  | Word32
  | Word64
  | Float
  | Double
  deriving (Show, Read)

data Const
  = CBool   Bool
  | CInt8   Int8
  | CInt16  Int16
  | CInt32  Int32
  | CInt64  Int64
  | CWord8  Word8
  | CWord16 Word16
  | CWord32 Word32
  | CWord64 Word64
  | CFloat  Float
  | CDouble Double
  deriving (Eq, Ord)

instance Show Const where
  show c = case c of
    CBool   True  -> "true"
    CBool   False -> "false"
    CInt8   c -> show c
    CInt16  c -> show c
    CInt32  c -> show c
    CInt64  c -> show c
    CWord8  c -> show c
    CWord16 c -> show c
    CWord32 c -> show c
    CWord64 c -> show c
    CFloat  c -> show c
    CDouble c -> show c

width :: Type -> Int
width a = case a of
  Bool   -> 1
  Int8   -> 8
  Int16  -> 16
  Int32  -> 32
  Int64  -> 64
  Word8  -> 8
  Word16 -> 16
  Word32 -> 32
  Word64 -> 64
  Float  -> 32
  Double -> 64



saveConfig :: Bus -> FilePath -> IO ()
saveConfig bus file = sendRequest >>= writeFile file . format
  where
  format :: [Word8] -> String
  format bytes = [ chr $ fromIntegral a | a <- bytes, a /= 0 ]

  sendRequest :: IO [Word8]
  sendRequest = do
    t <- busTime' bus
    sendMsg bus $ Msg 0x18ff00fa $ replicate 8 0
    getConfigHeader t

  getConfigHeader :: Int -> IO [Word8]
  getConfigHeader t = do
    a <- recvMsg bus
    case a of 
      Just (t, Msg 0x18ff01ef payload) -> getConfigData t (header payload) []
      Just (t', _) | t' - t > 100 -> printf "timeout on config header, restarting ...\n" >> sendRequest
      _ -> getConfigHeader t

  getConfigData :: Int -> (Int, Word32) -> [Word8] -> IO [Word8]
  getConfigData t (n, crc) buf
    | n * 8 == length buf && crc32 buf == crc = putStrLn "complete" >> return buf
    | n * 8 == length buf = putStrLn "crc failed, restarting ..." >> sendRequest
    | n * 8 <  length buf = putStrLn "received too many configuration packets, restarting ..." >> sendRequest
    | otherwise = do
    a <- recvMsg bus
    case a of 
      Just (t, Msg 0x18ff02ef payload) -> do
        printf "received %3d of %3d probed configuration packets ...\n" (div (length buf) 8 + 1) n
        getConfigData t (n, crc) (buf ++ payload)
      Just (t', _) | t' - t > 100 -> putStrLn "timeout on config data, restarting ..." >> sendRequest
      _ -> getConfigData t (n, crc) buf

header :: [Word8] -> (Int, Word32)
header payload' = (fromIntegral $ shiftR payload 32, fromIntegral payload)
  where
  payload = fromPayload payload'

startProbes :: Bus -> FilePath -> IO ()
startProbes bus file = do
  f <- readFile file
  vcd <- newVCD stdout MS
  sample <- sampleProbes vcd $ read f
  baseTime <- baseTime
  step vcd 0
  loop vcd sample baseTime
  where
  baseTime :: IO Int
  baseTime = do
    m <- recvMsgWait bus 1000
    case m of
      Just (t, _) -> return t
      _ -> baseTime
  loop :: VCDHandle -> (Int -> Word64 -> IO ()) -> Int -> IO ()
  loop vcd sample lastTime = do
    m <- recvMsgWait bus 1000
    case m of
      Just (time, Msg id payload) | id .&. 0xffff00ff == 0x18ff00ef && id' >= 3 && id' <= 127 -> do
        --printf "%-20d %-20d\n" time (time - lastTime)
        --hFlush stdout
        sample (id' - 3) $ fromPayload payload
        if time == lastTime
          then loop vcd sample lastTime
          else do
            step vcd $ time - lastTime
            loop vcd sample time
	where
        id' = fromIntegral $ shiftR id 8 .&. 0xFF

      _ -> loop vcd sample lastTime

sampleProbes :: VCDHandle -> [[(String, Type)]] -> IO (Int -> Word64 -> IO ())
sampleProbes vcd packedProbes = f1 0 packedProbes
  where
  f1 :: Int -> [[(String, Type)]] -> IO (Int -> Word64 -> IO ())
  f1 _ [] = return $ \ _ _ -> return ()
  f1 index (packet : probes) = do
    a <- f2 64 packet
    b <- f1 (index + 1) probes
    return $ \ i p -> when (i == index) (a p) >> b i p
    where
    f2 :: Int -> [(String, Type)] -> IO (Word64 -> IO ())
    f2 _ [] = return $ \ _ -> return ()
    f2 bit ((name, typ) : probes) = do
      a <- case typ of
        Bool   -> do
          f <- var vcd [name] False
          return $ \ a -> f $ testBit a (bit - 1)
        Int8   -> do
          f <- var vcd [name] (0 :: Int8  )
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit -  8
        Int16  -> do
          f <- var vcd [name] (0 :: Int16 )
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit - 16
        Int32  -> do
          f <- var vcd [name] (0 :: Int32 )
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit - 32
        Int64  -> do
          f <- var vcd [name] (0 :: Int64 )
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit - 64
        Word8  -> do
          f <- var vcd [name] (0 :: Word8 )
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit -  8
        Word16 -> do
          f <- var vcd [name] (0 :: Word16)
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit - 16
        Word32 -> do
          f <- var vcd [name] (0 :: Word32)
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit - 32
        Word64 -> do
          f <- var vcd [name] (0 :: Word64)
          return $ \ a -> f $ fromIntegral $ shiftR a $ bit - 64
        Float  -> do
          f <- var vcd [name] (0 :: Float )
          return $ \ a -> f $ toFloat (fromIntegral $ shiftR a $ bit - 32 :: Word32)
        Double -> do
          f <- var vcd [name] (0 :: Double)
          return $ \ a -> f $ toDouble (fromIntegral $ shiftR a $ bit - 64 :: Word64)
      b <- f2 (bit - width typ) probes
      return $ \ p -> a p >> b p

toFloat :: Word32 -> Float
toFloat = unsafeCoerce

toDouble :: Word64 -> Double
toDouble = unsafeCoerce

