module Main (main) where

import Data.Bits
import Data.Char
import Data.List
import Data.Word
import Text.Printf
import System.Environment

import CANData

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["-h"] -> help
    ["--help"] -> help
    [ file ] -> do
      dbc <- readFile file
      let candb = mkCANDB dbc
      writeCANDB candb
    _ -> help

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  parsedbc - create CANDB.hs from dbc file"
  , ""
  , "SYNOPSIS"
  , "  parsedbc FILE.dbc"
  , ""
  ]

writeCANDB :: CANDB -> IO ()
writeCANDB candb = writeFile "CANDB.hs" $ concat
  [ "{- Generated file. Do not modify -}\n"
  , "module CANDB\n"
  , "  ( canDB\n"
  , "  ) where\n"
  , "\n"
  , "import CANData\n"
  , "\n"
  , "canDB :: CANDB\n"
  , "canDB =  CANDB\n"
  , showCANDB candb
  , "\n"
  ]

mkCANDB :: String -> CANDB
mkCANDB s =  mkCANDB' nodes groups types attrs (CANDB name nodes [])
  where
  canLines = filter notBlank (lines s)
  name   = getName canLines
  groups = getGroups canLines
  nodes = getNodes canLines
  types = getTypes canLines
  attrs = getMsgAttrs canLines

mkCANDB' :: [String] -> [[String]] -> [(Word32,String,CANSignalType)] -> [(Word32,CANMsgAttr)] -> CANDB -> CANDB
mkCANDB' _ [] _ _ canDb = canDb
mkCANDB' nodes (group0:groups) types attrs canDb = mkCANDB' nodes groups types attrs (fillDB group0 types attrs canDb)

fillDB :: [String] -> [(Word32,String,CANSignalType)] -> [(Word32,CANMsgAttr)] -> CANDB -> CANDB
fillDB group0 types attrs canDB = getSignals types (tail group0) (getMessage (head group0) attrs canDB)

getMessage :: String -> [(Word32,CANMsgAttr)] -> CANDB -> CANDB
getMessage s attrs (CANDB dbname nodes msgs) = CANDB dbname nodes newMessages
  where
  newMessages = (msg:msgs)
  msg = CANMsg 
    { canMsgId      = id0
    , canMsgName    = init c
    , canMsgDlc     = read d
    , canMsgTxNode  = e
    , canMsgAttrs   = map (\ (_,a) -> a) (filter (\ (i,_) -> i == id0) attrs)
    , canMsgSignals = []
    }
  id0 = readId b
  (_:b:c:d:e:_) = words s

getSignals :: [(Word32,String,CANSignalType)] -> [String] -> CANDB -> CANDB
--getSignals _ [] (CANDB dbname nodes (msg:msgs)) = CANDB dbname nodes ((sortByStartBit msg):msgs)
getSignals _ [] canDb = canDb
getSignals types (l:ls) canDb = sortByStartBit (getSignal types l (getSignals types ls canDb))

sortByStartBit :: CANDB -> CANDB
sortByStartBit (CANDB dbname nodes []) = CANDB dbname nodes []
sortByStartBit (CANDB dbname nodes (msg:msgs)) = CANDB dbname nodes (newMsg:msgs)
  where
  newMsg = msg { canMsgSignals = sortBy comparingStartBit (canMsgSignals msg) }
  comparingStartBit :: CANSignal -> CANSignal -> Ordering
  comparingStartBit a b = compare (canSignalStartBit a) (canSignalStartBit b)

getSignal :: [(Word32,String,CANSignalType)] -> String -> CANDB -> CANDB
getSignal _ [] canDb = canDb
getSignal _ _ (CANDB _ _ []) = error "CANDBParser.getSignal"
getSignal types l (CANDB dbname nodes (msg:msgs)) = CANDB dbname nodes (newMsg:msgs) 
  where
  msgId = canMsgId msg
  newMsg = msg { canMsgSignals = newSignal:(canMsgSignals msg) }
  newSignal = CANSignal
    { canSignalName      = name
    , canSignalStartBit  = (read startBitStr)
    , canSignalBitLength = (read bitLengthStr)
    , canSignalEndian    = if bigEndianStr == '0' then CANBigEndian else CANLittleEndian
    , canSignalSign      = if signedStr == '-' then CANSigned else CANUnsigned
    , canSignalFactor    = (read (drop 1 (takeWhile (/= ',') e)))
    , canSignalOffset    = (read (takeWhile (/= ')') (drop 1 (dropWhile (/= ',') e))))
    , canSignalMin       = (read (drop 1 (takeWhile (/= '|') f)))
    , canSignalMax       = (read (takeWhile (/= ']') (drop 1 (dropWhile (/= '|') f))))
    , canSignalUnit      = unit
    , canSignalRxNodes   = rx
    , canSignalType      = matchTypes msgId name types
    , canSignalMux       = mux
    }
  startBitStr = takeWhile (/= '|') d
  bitLengthStr = drop 1 (dropWhile (/= '|') (takeWhile (/= '@') d))
  bigEndianStr = head (drop 1 (dropWhile (/= '@') d)) 
  signedStr    = last d
  (_:name:maybeMux) = takeWhile (/= ":") (words l)
  mux = case maybeMux of
    []             -> CANSignalMuxNone
    ["M"]          -> CANSignalMuxer
    [('m':muxVal)] -> CANSignalMuxed (read muxVal)
    _              -> error "CANDBParser.getSignal: error parsing \" SG_ ...\" line"
  (_:d:e:f:_) = dropWhile (/= ":") (words l)
  unitAndRx = dropWhile (/= '"') l
  unit = (takeWhile (/= '"') (drop 1 unitAndRx))
  rxlist = head (words (drop 1 (dropWhile (/= '"') (drop 1 unitAndRx))))
  rx = wordsBy ',' rxlist
  matchTypes :: Word32 -> String -> [(Word32,String,CANSignalType)] -> CANSignalType
  matchTypes _ _ [] = CANSignalTypeNormal
  matchTypes msgId0 sigName ((id0,name0,typ):types0) = 
    if (msgId0 == id0 && sigName == name0)
      then typ
      else matchTypes msgId0 sigName types0
  wordsBy :: Char -> [Char] -> [String]
  wordsBy sep s = case dropWhile (== sep) s of
    "" -> []
    s' -> w : wordsBy ',' s''
      where
      (w, s'') = break (== sep) s'

getName :: [String] -> String
getName [] = []
getName (l:ls) = 
  if isPrefixOf "BA_ \"DBName\" " l
    then (takeWhile (/= '"') (drop 1 c))
    else getName ls
  where
  (_:_:c:_) = words l

getNodes :: [String] -> [String]
getNodes (l:ls) = 
  if isPrefixOf "BU_: " l
    then (words (drop 5 l))
    else getNodes ls
getNodes [] = []

getTypes :: [String] -> [(Word32,String,CANSignalType)]
getTypes ls =  getTypes' ls []

getTypes' :: [String] -> [(Word32,String,CANSignalType)] -> [(Word32,String,CANSignalType)]
getTypes' [] types = types
getTypes' (l:ls) types = 
  if isPrefixOf "SIG_VALTYPE_ " l
    then getTypes' ls ((id0,name,typ):types)
    else getTypes' ls types
  where
  typ = if typChar == '1' then CANSignalTypeFloat else CANSignalTypeDouble
  typChar = head e
  id0 = read b
  name = c
  (_:b:c:_:e:_) =  words l

getMsgAttrs :: [String] -> [(Word32,CANMsgAttr)]
getMsgAttrs ls =  getMsgAttrs' ls []

getMsgAttrs' :: [String] -> [(Word32,CANMsgAttr)] -> [(Word32,CANMsgAttr)]
getMsgAttrs' [] attrs = attrs
getMsgAttrs' (l:ls) attrs = 
  if isPrefixOf "BA_ " l && length (words l) == 5
    then 
      if c == "BO_"
        then getMsgAttrs' ls ((id0,attr):attrs)
        else getMsgAttrs' ls attrs
    else getMsgAttrs' ls attrs
  where
  id0 = readId d
  attr = (tail (init b), init e)
  (_:b:c:d:e:_) = words l

getGroups :: [String] -> [[String]]
getGroups ls = groupMessages ls [[]]

groupMessages :: [String] -> [[String]] -> [[String]]
groupMessages [] [] = [[]]
groupMessages [] sofar = map reverse sofar
groupMessages (l:ls) ([]:[])  = case take 4 l of
  "BO_ " -> groupMessages ls [[l]]
  _      -> groupMessages ls [[]]
groupMessages (l:ls) ((s:ss):gs) = case (take 4 l) of
  "BO_ " -> groupMessages ls ([l]:(s:ss):gs)
  " SG_" -> groupMessages ls ((l:s:ss):gs)
  _      -> reverse (map reverse ((s:ss):gs))
groupMessages _ [] = error "CANDBParser.groupMessages"
groupMessages _ ([]:_) = error "CANDBParser.groupMessages"

notBlank :: String -> Bool
notBlank a = not (and (map isSpace a))

readId :: String -> Word32
readId str = (read str) .&. 0x7FFFFFFF

showCANDB :: CANDB -> String
showCANDB candb = concat
  [ "  { canDbName = " ++ show (canDbName candb) ++ "\n"
  , "  , canDbNodes =\n"
  , "      [" ++ (drop 7 (concatMap showNode (canDbNodes candb)))
  , "      ]\n"
  , "  , canDbMsgs =\n"
  , "      [" ++ (drop 7 (concatMap showMsg (canDbMsgs candb)))
  , "      ]\n"
  , "  }\n"
  ]

showNode :: String -> String
showNode x = "      , " ++ show x ++ "\n"

showMsg :: CANMsg -> String
showMsg msg = concat
  [ "      , CANMsg\n"
  , "          { canMsgId      = " ++ (printf "0x%08X" (canMsgId msg)) ++ "\n"
  , "          , canMsgName    = " ++ show (canMsgName msg) ++ "\n"
  , "          , canMsgDlc     = " ++ show (canMsgDlc msg) ++ "\n"
  , "          , canMsgTxNode  = " ++ show (canMsgTxNode msg) ++ "\n"
  , "          , canMsgAttrs   =\n"
  , "              [" ++ showMsgAttrs (canMsgAttrs msg)
  , "              ]\n"
  , "          , canMsgSignals =\n"
  , "              [" ++ showMsgSignals (canMsgSignals msg) ++ "\n"
  , "              ]\n"
  , "          }\n"
  ]

showMsgAttrs :: [CANMsgAttr] -> String
showMsgAttrs attrs = case attrs of
  [] -> ""
  x  -> drop 15 (concatMap showMsgAttr x)

showMsgAttr :: CANMsgAttr -> String
showMsgAttr (a , b)  = "              , (" ++ show a ++ ", " ++ show b ++ ")\n"

showMsgSignals :: [CANSignal] -> String
showMsgSignals signals = case signals of
  [] -> ""
  x  -> drop 15 (concatMap showMsgSignal x)

showMsgSignal :: CANSignal -> String
showMsgSignal signal = concat
  [ "              , CANSignal\n"
  , "                  { canSignalName      = " ++ show (canSignalName      signal) ++ "\n"
  , "                  , canSignalStartBit  = " ++ show (canSignalStartBit  signal) ++ "\n"
  , "                  , canSignalBitLength = " ++ show (canSignalBitLength signal) ++ "\n"
  , "                  , canSignalEndian    = " ++ show (canSignalEndian    signal) ++ "\n"
  , "                  , canSignalSign      = " ++ show (canSignalSign      signal) ++ "\n"
  , "                  , canSignalFactor    = " ++ show (canSignalFactor    signal) ++ "\n"
  , "                  , canSignalOffset    = " ++ show (canSignalOffset    signal) ++ "\n"
  , "                  , canSignalMin       = " ++ show (canSignalMin       signal) ++ "\n"
  , "                  , canSignalMax       = " ++ show (canSignalMax       signal) ++ "\n"
  , "                  , canSignalUnit      = " ++ show (canSignalUnit      signal) ++ "\n"
  , "                  , canSignalRxNodes   =\n"
  , "                      [" ++ showCANSignalRxNodes (canSignalRxNodes     signal)
  , "                      ]\n"
  , "                  , canSignalType      = " ++ show (canSignalType      signal) ++ "\n"
  , "                  , canSignalMux       = " ++ show (canSignalMux       signal) ++ "\n"
  , "                  }\n"
  ]

showCANSignalRxNodes :: [String] -> String
showCANSignalRxNodes nodes = case nodes of
  [] -> ""
  x  -> drop 23 (concatMap showCANSignalRxNode x)

showCANSignalRxNode :: String -> String
showCANSignalRxNode x  = "                      , " ++ show x ++ "\n"

