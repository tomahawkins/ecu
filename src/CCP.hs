module Main (main) where

import Control.Monad
import Data.Word
import System.Environment
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
  , "  upload { option } address           Upload 4 bytes from the ECU.  Address must be 32-bit aligned."
  , "  upload { option } address length    Upload a memory region from the ECU.  Address must be 32-bit aligned."
  , ""
  , "OPTIONS"
  , "  --std    Set to standard CAN with 500K.  Default is extended CAN with 250K."
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
    ["upload",          addr] -> f ["upload",          addr, "4"]
    ["upload", "--std", addr] -> f ["upload", "--std", addr, "4"]
    ["upload", "--std", addr, size] -> do
      initCAN
      bus <- openBus 0 Standard
      ids <- ids
      upload bus ids (read addr) (read size)
      closeBus bus
    ["upload", addr, size] -> do
      initCAN
      bus <- openBus 0 Extended
      ids <- ids
      upload bus ids (read addr) (read size)
      closeBus bus
    _ -> help

canview :: Bus -> [Word32] -> IO ()
canview bus ids = do
  m <- recvMsg bus
  case m of
    Nothing -> canview bus ids
    Just (t, m@(Msg id _)) -> do
      when (null ids || elem id ids) $ putStrLn $ printf "%10i   " t ++ show m
      canview bus ids

ids :: IO (Word32, Word32)
ids = do
  master <- getEnv "CCP_ID_MASTER"
  slave  <- getEnv "CCP_ID_SLAVE"
  return (read master, read slave)

upload :: Bus -> (Word32, Word32) -> Word32 -> Word32 -> IO ()
upload bus (master, slave) address size = do
  printf "%08x  %08x\n" master slave
  return ()

