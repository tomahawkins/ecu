module Main (main) where

import Control.Monad
import Data.Word
import System.Environment
import System.IO
import Text.Printf

import CAN

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> help
    ["--help"] -> help
    "--std" : ids -> do
      initCAN
      bus <- openBus 0 Standard
      flushRxQueue bus
      canview bus $ map read ids
    ids -> do
      initCAN
      bus <- openBus 0 Extended
      flushRxQueue bus
      canview bus $ map read ids

canview :: Bus -> [Word32] -> IO ()
canview bus ids = do
  m <- recvMsg bus
  case m of
    Nothing -> canview bus ids
    Just (t, m@(Msg id _)) -> do
      when (null ids || elem id ids) $ do
        putStrLn $ printf "%10i   " t ++ show m
	hFlush stdout
      canview bus ids

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  canview - listens to a CAN bus"
  , ""
  , "SYNOPSIS"
  , "  canview [ --std ] { <id-filter> }"
  , ""
  , "ARGUMENTS"
  , "  --std          Set to standard CAN with 500K.  Default is extended CAN with 250K."
  , ""
  , "  <id-filter>    If id filters are included, only matching ids will be displayed." 
  , ""
  ]

