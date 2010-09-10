module Main (main) where

import System.Environment
import Text.Printf

import CAN

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--std"] -> do
      initCAN
      bus <- openBus 0 Standard
      flushRxQueue bus
      canview bus
    [] -> do
      initCAN
      bus <- openBus 0 Extended
      flushRxQueue bus
      canview bus
    _ -> help

canview :: Bus -> IO ()
canview bus = do
  m <- recvMsg bus
  case m of
    Nothing -> canview bus
    Just (t, m) -> do
      putStrLn $ printf "%10i   " t ++ show m
      canview bus

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  canview - listens to a CAN bus"
  , ""
  , "SYNOPSIS"
  , "  canview {argument}"
  , ""
  , "ARGUMENTS"
  , "  --std          Set to standard CAN with 500K.  Default is extended CAN with 250K."
  , ""
  ]

