module Main (main) where

import Control.Monad
import Data.List
import Data.Maybe
import System.Directory
import System.Environment
import System.IO
import System.Process
import Text.Printf

help :: IO ()
help = putStrLn $ unlines
  [ ""
  , "NAME"
  , "  git2cc - bridges git repository to ClearCase"
  , ""
  , "SYNOPSIS"
  , "  git2cc <git-directory> <cc-view-directory>"
  , ""
  ]

{-
Git Notes

git clone -q -n rcs@foell.org:git/eaton  # Index but no working tree.
git checkout -q -f <commit>              # Populate working tree will to a commit.
git log --decorate                       # List tags next to commit.
  commit 123412341234 (HEAD, 1.0.3, ...
  commit 123412341234 (1.0.2)

git commit -m   # Can do multiple -m <msg> arguments.  Use one for ETN activity number.
-}

main :: IO ()
main = do
  args <- getArgs
  case args of
    [repo, '/':view] -> inDirectory repo $ push $ '/' : view
    [repo, view] -> do
      d <- getCurrentDirectory
      inDirectory repo $ push $ d ++ "/" ++ view
    _ -> help

push :: FilePath -> IO ()
push view = do
  lastCommit <- lastCommit view
  putStrLn $ "last commit: " ++ lastCommit
  hFlush stdout
  commits <- getCommits >>= return . reverse . takeWhile ((/= lastCommit) . commit)
  mapM_ (applyCommit view (status $ length commits)) $ zip [1..] commits

exec :: String -> IO ()
exec cmd = do
  putStrLn cmd
  hFlush stdout
  system cmd
  return ()

applyCommit :: FilePath -> (Int -> String) -> (Int, Commit) -> IO ()
applyCommit view status (i, c) = do
  printf "%s  commit %s  %s  %s\n" (status i) (commit c) (activity c) (intercalate ", " $ tags c)
  hFlush stdout
  exec $ "git checkout -q -f " ++ commit c
  from <- hashDirectory view
  to   <- hashDirectory "."
  let cmds = [SetAct (activity c)] ++ compileCommit "./" from to ++ [CO [".commit"], Version (commit c), CI [".commit"]] ++ map BL (tags c)
  mapM_ exec $ commands view cmds

status :: Int -> Int -> String
status a b = printf f b a
  where
  l = length $ show a
  f = "[%" ++ show l ++ "i of %" ++ show l ++ "i]"

lastCommit :: FilePath -> IO String
lastCommit view = inDirectory view $ do
  d <- getCurrentDirectory
  c <- getDirectoryContents d
  when (not $ elem ".commit" c) $ do
    putStrLn "creating blank .commit file ..."
    hFlush stdout
    exec "cleartool co -nc ."
    exec "touch .commit"
    exec "cleartool mkelem -nc .commit"
    exec "cleartool ci -identical -nc .commit"
    exec "cleartool ci -identical -nc ."
  a <- readProcess "cat" [".commit"] ""
  return $ if null a then a else init a

inDirectory :: FilePath -> IO a -> IO a
inDirectory path action = do
  home <- getCurrentDirectory
  setCurrentDirectory path
  a <- action
  setCurrentDirectory home
  return a

getCommits :: IO [Commit]
getCommits = readProcess "git" ["log", "--decorate=full"] "" >>= return . parseCommits


data Cmd
  = SetAct String
  | BL String
  | CO [FilePath]
  | CI [FilePath]
  | RM FilePath
  | MK FilePath
  | Version String
  | Mkdir FilePath
  | CP FilePath
  | Note String

commands :: FilePath -> [Cmd] -> [String]
commands view cmds = map command cmds
  where
  command :: Cmd -> String
  command cmd = case cmd of
    SetAct a       -> "cd " ++ view ++ " && cleartool setact " ++ a
    BL baseline    -> "cd " ++ view ++ " && cleartool mkbl -all -identical -full " ++ baseline ++ " && cleartool chstream -recommended " ++ baseline ++ " -cview"
    CO files       -> "cd " ++ view ++ " && cleartool co -nc "            ++ intercalate " " files
    CI files       -> "cd " ++ view ++ " && cleartool ci -identical -nc " ++ intercalate " " files
    RM file        -> "cd " ++ view ++ " && cleartool rmname " ++ file ++ " && rm " ++ file
    MK file        -> "cd " ++ view ++ " && cleartool mkelem -nc " ++ file
    Version commit -> "cd " ++ view ++ " && echo " ++ show commit ++ " > .commit"
    Mkdir dir      -> "cd " ++ view ++ " && mkdir " ++ dir
    CP file        -> "cp " ++ file ++ " " ++ view ++ "/" ++ file
    Note note      -> "echo " ++ show note


data Hash = Directory String [Hash] | File String String deriving (Show, Eq)

hashDirectory :: FilePath -> IO [Hash]
hashDirectory d = inDirectory d $ do
  a <- getDirectoryContents "."
  a <- mapM hashFile a
  return $ catMaybes a

hashFile :: FilePath -> IO (Maybe Hash)
hashFile file | elem file [".", "..", ".git", ".commit"] = return Nothing
hashFile file = do
  d <- doesDirectoryExist file
  if d
    then do
      a <- hashDirectory file
      return $ Just $ Directory file a
    else do
      checksum <- readProcess "md5sum" [file] ""
      return $ Just $ File file checksum

compileCommit :: FilePath -> [Hash] -> [Hash] -> [Cmd]
compileCommit path from to = concatMap patch' (align from to)
  where
  patch' :: (Maybe Hash, Maybe Hash) -> [Cmd]
  patch' (from, to) = case (from, to) of

    -- modifications
    (Just (Directory name a), Just (Directory _ b)) -> compileCommit (path ++ name ++ "/") a b
    (Just (File      name a), Just (File      _ b)) | a /= b    -> [Note $ "modifying file: " ++ path ++ name, CO [path ++ name], CP (path ++ name), CI [path ++ name]]
                                                    | otherwise -> []

    -- removals
    (Just (Directory name _), Nothing) -> [Note $ "removing directory: " ++ path ++ name, CO [path], RM (path ++ name), CI [path]]
    (Just (File      name _), Nothing) -> [Note $ "removing file: "      ++ path ++ name, CO [path], RM (path ++ name), CI [path]]

    -- additions
    (Nothing, Just (Directory name a)) -> [Note $ "adding directory: " ++ path ++ name, CO [path], Mkdir (path ++ name), MK (path ++ name), CI [path ++ name, path]] ++ compileCommit (path ++ name ++ "/") [] a
    (Nothing, Just (File      name _)) -> [Note $ "adding file: "      ++ path ++ name, CO [path], CP (path ++ name),    MK (path ++ name), CI [path ++ name, path]]

    (a, b) -> error "patch: unexpected diff pair: " $ show (a, b)
  
  
align :: [Hash] -> [Hash] -> [(Maybe Hash, Maybe Hash)]
align [] a = zip (replicate (length a) Nothing) (map Just a)
align a [] = zip (map Just a) (replicate (length a) Nothing)
align (file@(File name _) : a) b = (Just file, file') : align a b'
  where
  f (File n _) = n == name
  f _ = False
  (file', b') = getHash f b
align (dir@(Directory name _) : a) b = (Just dir, dir') : align a b'
  where
  f (Directory n _) = n == name
  f _ = False
  (dir', b') = getHash f b

getHash :: (Hash -> Bool) -> [Hash] -> (Maybe Hash, [Hash])
getHash _ [] = (Nothing, [])
getHash f (a:b) | f a = (Just a, b)
                | otherwise = (a', a:b') where (a', b') = getHash f b


data Commit = Commit
  { commit   :: String
  , activity :: String
  , tags     :: [String]
  } deriving Show

-- Parse string from 'git log --decorate=full'.
parseCommits :: String -> [Commit]
parseCommits = parseCommits . lines
  where
  
  parseCommits :: [String] -> [Commit]
  parseCommits [] = []
  parseCommits (a:b) | isPrefixOf "commit " a = Commit { commit = commit', activity = activity', tags = tags' } : commits
                     | otherwise = parseCommits b
    where
    commit' = take 40 $ drop 7 $ a
    tags'   = parseTags $ drop 47 a
    (activity', commits) = parseActivityCommits b

  parseTags :: String -> [String]
  parseTags [] = []
  parseTags a | isPrefixOf "refs/tags/" a = tag : parseTags rest
              | otherwise = parseTags $ tail a
    where
    tag = takeWhile (flip notElem ",)") $ drop 10 a
    rest = drop (10 + length tag) a

  parseActivityCommits :: [String] -> (String, [Commit])
  parseActivityCommits [] = ("ETN00000000", [])  -- Misc activity.
  parseActivityCommits s@(a:b) | isPrefixOf "    ETN" a = (take 11 $ drop 4 a, parseCommits b)
                               | isPrefixOf "commit " a = (activity $ head $ parseCommits s, parseCommits s)
                               | otherwise              = parseActivityCommits b

