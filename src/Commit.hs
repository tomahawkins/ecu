module Main (main) where

import Data.Char
import Data.List
import System.Environment
import System.Exit
import System.Process

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  commit - Git commits with ClearQuest activities"
  , ""
  , "SYNOPSIS"
  , "  commit [ --cqact=<activity id> ] { <git commit arguments> }"
  , ""
  , "DESCRIPTION"
  , "  Performs a git commit with a comment to capture ClearQuest activity."
  , "  Activity format must be: ETNnnnnnnnn"
  , ""
  , "OPTIONS"
  , "  --cqact=<activity id>    Overrides activity id set by CQACT environment variable."
  , ""
  ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> help
    ("-h":_) -> help
    ("--help":_) -> help
    (a:args) | isPrefixOf "--cqact=" a -> if isActivity act then commit act args else error $ "invalid activity format: " ++ act
      where
      act = drop 8 a
    args -> do
      env <- getEnvironment
      case lookup "CQACT" env of
        Just a | isActivity a -> commit a args
        _ -> error "CQACT not set or not formated correctly"
      
isActivity :: String -> Bool
isActivity ('E':'T':'N':a) = length a == 8 && all isDigit a
isActivity _ = False

commit :: String -> [String] -> IO ()
commit activity args = do
  putStrLn cmd
  system cmd >>= exitWith
  where
  cmd = "git commit -m " ++ activity ++ " " ++ intercalate " " (map format args)

format :: String -> String
format a | any (flip elem " \"") a = "\"" ++ concat [ if c == '"' then "\\\"" else [c] | c <- a ] ++ "\""
         | otherwise = a



