module Main (main) where

import Data.Bits
import qualified Data.ByteString.Lazy as B
import Data.IORef
import Data.VCD hiding (Bool)
import Data.Word
import System.Environment
import System.IO
import Text.Printf

main :: IO ()
main = do
  args <- getArgs
  case args of
    ('-' : _) : _ -> help
    [dbc, "-"] -> do
      dbc <- readFile dbc
      cl <- B.getContents >>= return . B.unpack
      buildVCD (parseDBC dbc) (parseCL cl) stdout
    dbc : cls -> do
      dbc <- readFile dbc
      mapM_ (buildVCDFile $ parseDBC dbc) cls
    _ -> help

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "  NAME"
  , "    tovcd - converts dbc and cl files to vcd"
  , ""
  , "  SYNOPSIS"
  , "    tovcd <dbc-file> ( - | { <cl-file> } )"
  , ""
  , "  OPTIONS"
  , "    -   Read cl from stdin, vcd written to stdout."
  , ""
  ]

-- Generate VCD to a handle.
buildVCD :: DBC -> [(Word32, Word32, Word64)] -> Handle -> IO ()
buildVCD dbc cl h = do
  vcd <- newVCD h MS
  sample <- mapM (sampleMsg vcd) dbc
  mapM_ (\ a -> sequence_ [ f a | f <- sample ]) cl

-- Sets up the VCD sampling functions for each field in each message.
sampleMsg :: VCDHandle -> Msg -> IO ((Word32, Word32, Word64) -> IO ())
sampleMsg vcd (Msg id fields) = do
  sampleFields <- mapM (sampleField vcd) fields
  t <- newIORef 0
  return $ \ (time, id', payload) -> if id /= id' then return () else do
    t' <- readIORef t
    step vcd $ fromIntegral (time - t')
    writeIORef t time
    sequence_ [ f payload | f <- sampleFields ]

-- Sample an individual field from a payload.
sampleField :: VCDHandle -> Field -> IO (Word64 -> IO ())
sampleField vcd field = case field of
  Bool name bit -> do
    v <- var vcd name False
    return $ \ payload -> v $ testBit payload $ 64 - bit
  Float name bit width gain bias -> do
    v <- var vcd name (0 :: Float)
    return $ \ payload -> v $ fromIntegral (shiftR payload (71 - width - bit) .&. mask width) * gain + bias
  where
  mask :: Int -> Word64
  mask n = if n <= 0 then 0 else 1 .|. shiftL (mask $ n - 1) 1

-- Generate a VCD to a file.
buildVCDFile :: DBC -> FilePath -> IO ()
buildVCDFile dbc clFile = do
  putStrLn $ "converting " ++ clFile ++ " to " ++ vcdFile ++ " ..."
  cl <- B.readFile clFile >>= return . B.unpack
  h <- openFile vcdFile WriteMode
  buildVCD dbc (parseCL cl) h
  hClose h
  where
  vcdFile = if elem '.' clFile then reverse (dropWhile (/= '.') (reverse clFile)) ++ "vcd" else clFile ++ ".vcd"

-- DBC is a list of messages.
type DBC = [Msg]

-- A message is a is a CAN ID and a list of fields.
data Msg = Msg Word32 [Field]

instance Show Msg where
  show (Msg id fields) = printf "Msg %08x\n" id ++ concat [ "  " ++ show f ++ "\n" | f <- fields ]

-- A field is either a boolean (name, bit position) or a float (name, bit position, width, scaling, and offset).
data Field
  = Bool String Int
  | Float String Int Int Float Float
  deriving Show

parseDBC :: String -> DBC
parseDBC = parseMsgs . lines

parseMsgs :: [String] -> [Msg]
parseMsgs [] = []
parseMsgs (a : b)
  | not (null w) && head w == "BO_" = Msg (0x7fffffff .&. read (w !! 1)) fields : parseMsgs rest
  | otherwise = parseMsgs b
  where
  w = words a
  (fields, rest) = parseFields b

parseFields :: [String] -> ([Field], [String])
parseFields [] = ([], [])
parseFields (a : b)
  | not (null w) && head w == "SG_" = (field : fields, rest)
  | otherwise = ([], a : b)
  where
  w = words $ map (\ a -> if elem a "|@(,)" then ' ' else a) a
  (fields, rest) = parseFields b
  name  = w !! 1
  bit   = read $ w !! 3
  width = read $ w !! 4
  gain  = read $ w !! 6
  bias  = read $ w !! 7
  field = case width of
    1 -> Bool name bit
    _ -> Float name bit width gain bias

parseCL :: [Word8] -> [(Word32, Word32, Word64)]
parseCL = f
  where
  f (a0:a1:a2:a3:a4:a5:a6:a7:a8:a9:aa:ab:ac:ad:ae:af:rest) =
    (word32 (a3, a2, a1, a0), word32 (a7, a6, a5, a4), word64 (a8, a9, aa, ab, ac, ad, ae, af)) : f rest
  f _ = []

-- | Assumes big endian.  Flip order if data is little endian.
word32 :: (Word8, Word8, Word8, Word8) -> Word32
word32 (a0,a1,a2,a3) = shiftL (fromIntegral a0) 24
                   .|. shiftL (fromIntegral a1) 16
                   .|. shiftL (fromIntegral a2)  8
                   .|.        (fromIntegral a3)   

-- | Assumes big endian.  Flip order if data is little endian.
word64 :: (Word8, Word8, Word8, Word8, Word8, Word8, Word8, Word8) -> Word64
word64 (a0,a1,a2,a3,a4,a5,a6,a7) = shiftL (fromIntegral a0) 56
                               .|. shiftL (fromIntegral a1) 48
                               .|. shiftL (fromIntegral a2) 40
                               .|. shiftL (fromIntegral a3) 32
                               .|. shiftL (fromIntegral a4) 24
                               .|. shiftL (fromIntegral a5) 16
                               .|. shiftL (fromIntegral a6)  8
                               .|.        (fromIntegral a7)   



