module Main (main) where

import Data.Bits
import qualified Data.ByteString as BS
import Data.Char
import Data.Digest.CRC32
import Data.List
import Data.Word
import System.Environment
import Text.Printf

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "  NAME"
  , "    toesb - converts an s-record ecu image to the Eaton Standard Binary (ESB) format"
  , ""
  , "  SYNOPSIS"
  , "    toesb [ --nocrc ] <input-file>"
  , ""
  , "  OPTIONS"
  , "    --nocrc   Ignore CRC checking on s-record."
  , ""
  ]


main :: IO ()
main = do
  args <- getArgs
  case args of
    [i]            -> readFile i >>= srec2esb (out i) True
    ["--nocrc", i] -> readFile i >>= srec2esb (out i) False
    _ -> help

out :: FilePath -> FilePath
out a = takeWhile (/= '.') a ++ ".esb"

srec2esb :: FilePath -> Bool -> String -> IO ()
srec2esb o checkCRC a = do
  printf "section address    section length\n"
  sequence_ [ printf "0x%08X         0x%08X\n" a (length b) | (a, b) <- a3 ]
  putStrLn $ "writing " ++ o ++ " ..."
  BS.writeFile o $ BS.pack $ map fromIntegral file
  where
  a1 = [ (dehex $ init $ init $ drop 2 a, head $ dehex $ reverse $ take 2 $ reverse a) | l <- lines a, let a = filter isAlphaNum l, isPrefixOf "S3" a ]
  a2 = concatMap (addr checkCRC) a1
  a3 = [ (a, b) | (a, b) <- blocks a2, a >= 0x10000, a <= 0x7FFFF ]
  n = length a3
  allBlocks = le n ++ concatMap j1939Block a3
  fileCRC = fromIntegral $ crc32 $ (map fromIntegral allBlocks :: [Word8])
  file = [0xE5, 0x5B, 0xBE, 0xE5, 5, 0, 0, 0] ++ le fileCRC ++ allBlocks
 
addr :: Bool -> ([Int], Int) -> [(Int, Int)]  -- (addr, byte)
addr checkCRC (a, checksum) = if checkCRC && complement (sum a) .&. 0xFF /= checksum then error $ "failed checksum: " ++ show (a, checksum) else zip addrs bytes
  where
  base = shiftL (a !! 1) 24
       + shiftL (a !! 2) 16
       + shiftL (a !! 3)  8
       +        (a !! 4)
  addrs  = [base ..]
  bytes = drop 5 a

blocks :: [(Int, a)] -> [(Int, [a])]
blocks [] = []
blocks ((addr, byte) : rest) = section : blocks rest'
  where
  (section, rest') = block (addr + 1) (addr, [byte]) rest

block :: Int -> (Int, [a]) -> [(Int, a)] -> ((Int, [a]), [(Int, a)])
block nextAddr (startingAddr, sofar) ((a, b):rest) | a == nextAddr = block (nextAddr + 1) (startingAddr, b : sofar) rest
block _ (a, b) rest = ((a, reverse b), rest)

dehex :: String -> [Int]
dehex "" = []
dehex (a:b:c) = read ("0x" ++ [a,b]) : dehex c
dehex _ = undefined


j1939Block :: (Int, [Int]) -> [Int]
j1939Block (addr, d) = [3, 0, 0, 0] ++ le (shiftL (addr - 0x10000) 8 .&. 0xFFFFFF00 .|. 128) ++ le (length d) ++ d

le :: Int -> [Int]
le a = [       a    .&. 0xFF]
    ++ [shiftR a  8 .&. 0xFF]
    ++ [shiftR a 16 .&. 0xFF]
    ++ [shiftR a 24 .&. 0xFF]
