module Main (main) where

import System.Environment

import CAN

main :: IO ()
main = do
  args <- getArgs
  case args of
    "-h" : _ -> help
    "--help" : _ -> help
    ["--std", id, payload] -> do
      initCAN
      bus <- openBus 0 Standard
      sendMsg bus $ Msg (read id) (toPayload $ read payload)
      closeBus bus
    [id, payload] -> do
      initCAN
      bus <- openBus 0 Extended
      sendMsg bus $ Msg (read id) (toPayload $ read payload)
      closeBus bus
    _ -> help

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  cansend - puts a message on a CAN bus"
  , ""
  , "SYNOPSIS"
  , "  cansend [--std] id payload"
  , ""
  , "ARGUMENTS"
  , "  --std          Set to standard CAN with 500K.  Default is extended CAN with 250K."
  , ""
  ]

