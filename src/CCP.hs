module Main (main) where

import Control.Monad
import Data.IORef
import Data.Word
import System.Environment
import System.IO
import Text.Printf

import CAN

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  ccp - a CAN Configuration Protocol master"
  , ""
  , "SYNOPSIS"
  , "  ccp command { option } { argument }"
  , ""
  , "COMMANDS"
  , "  upload address           Upload 4 bytes from the ECU."
  , "  upload address length    Upload a memory region from the ECU."
  , ""
  , "OPTIONS"
  , "  -v  Verbose.  Show message transactions."
  , ""
  , "ENVIRONMENT VARIABLES"
  , "  CCP_ID_MASTER    CAN identifier for master device (this configuration tool)."
  , "  CCP_ID_SLAVE     CAN identifier for slave device (ecu)."
  , ""
  ]

main :: IO ()
main = getArgs >>= f
  where
  f args = case args of
    ["-h"] -> help
    ["--help"] -> help
    ["upload", "-v", addr] -> f ["upload", "-v", addr, "4"]
    ["upload", "-v", addr, size] -> do
      connection <- connect True
      upload connection (read addr) (read size)
      disconnect connection
    ["upload", addr] -> f ["upload", addr, "4"]
    ["upload", addr, size] -> do
      connection <- connect False
      upload connection (read addr) (read size)
      disconnect connection
    _ -> help

data Connection = Connection
  { cBus     :: Bus
  , cMaster  :: Word32
  , cSlave   :: Word32
  , cCounter :: IORef Word8
  , cVerbose ::  Bool
  }

connect :: Bool -> IO Connection
connect verbose = do
  initCAN
  bus     <- openBus 0 Extended
  flushRxQueue bus
  master  <- getEnv "CCP_ID_MASTER"
  slave   <- getEnv "CCP_ID_SLAVE"
  counter <- newIORef 0
  return Connection
    { cBus     = bus
    , cMaster  = read master
    , cSlave   = read slave
    , cCounter = counter
    , cVerbose = verbose
    }

disconnect :: Connection -> IO ()
disconnect c = closeBus $ cBus c

transact :: Connection -> Word8 -> [Word8] -> IO (Maybe [Word8])
transact c command payload = do
  counter <- readIORef $ cCounter c
  writeIORef (cCounter c) $ counter + 1
  let msg = Msg (cMaster c) $ take 8 $ command : counter : payload ++ repeat 0xff
  when (cVerbose c) $ do
    printf "send: %s\n" $ show msg
    hFlush stdout
  sendMsg (cBus c) msg
  time <- busTime' $ cBus c
  recv counter time
  where
  recv :: Word8 -> Int -> IO (Maybe [Word8])
  recv counter time = do
    m <- recvMsgWait (cBus c) 25    
    case m of
      Nothing -> return Nothing
      Just (t, m@(Msg id payload))
        | id == cSlave c && length payload == 8 && payload !! 0 == 0xff && payload !! 2 == counter -> do
            if payload !! 1 == 0
              then do
                when (cVerbose c) $ do
                  printf "recv: %s\n" (show m)
                  hFlush stdout
                return $ Just $ drop 3 payload
              else error $ printf "error code: %02x\n" $ payload !! 1
        | t - time > 25 -> return Nothing
        | otherwise -> recv counter time

transactRetry :: Int -> Connection -> Word8 -> [Word8] -> IO [Word8]
transactRetry n c cmd payload
  | n <= 0 = error "transactRetry failed to complete"
  | otherwise = do
      a <- transact c cmd payload
      case a of
        Nothing -> transactRetry (n - 1) c cmd payload
        Just a  -> return a

setMTA0 :: Connection -> Word32 -> IO ()
setMTA0 c address = do
  transactRetry 5 c 0x02 $ 0 : 0 : fromWord32 address
  return ()

upload :: Connection -> Word32 -> Word32 -> IO ()
upload c address size = do
  bytes <- upload0 address size
  sequence_ [ printf "%08x  %s\n" address value | (address, value) <- zip [address, address + 4 ..] (pack $ map (printf "%02x") bytes :: [String]) ]
  where
  upload0 :: Word32 -> Word32 -> IO [Word8]
  upload0 address size
    | size <= 5 = do
      a <- transact c 0x0f $ fromIntegral size : 0 : fromWord32 address
      case a of
        Nothing -> do
          when (cVerbose c) $ do
            printf "upload restart at address %08x\n" address
            hFlush stdout
          upload0 address size
        Just payload -> return $ take (fromIntegral size) payload
    | otherwise = do
      setMTA0 c address
      upload1 address size

  upload1 :: Word32 -> Word32 -> IO [Word8]
  upload1 address size = if size == 0 then return [] else do
    a <- transact c 0x04 [if size > 5 then 5 else fromIntegral size]
    case a of
      Nothing -> do
        when (cVerbose c) $ do
          printf "upload restart at address %08x\n" address
          hFlush stdout
        upload0 address size
      Just payload -> do
        a <- upload1 (address + min size 5) (max size 5 - 5)
        return $ take (fromIntegral size) payload ++ a

  pack :: [String] -> [String]
  pack (a0 : a1 : a2 : a3 : rest) = concat [a0, a1, a2, a3] : pack rest
  pack [] = []
  pack a = [concat a]
       

