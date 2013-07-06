module Main where

import System.Environment (getArgs, getProgName, lookupEnv)
import Control.Exception (bracketOnError)
import System.IO (openTempFile, Handle, hClose, hPutStr, hFlush, stdout)
import System.Directory (removeFile, renameFile, doesFileExist)
import Data.List (delete)
import Control.Monad (liftM, unless)
import System.FilePath ((</>))
import Safe (readMay, atMay)
import System.Exit (exitSuccess)

------------ Config ------------

-- | Change this to Just a filepath to specify the location of your todo list
-- files. Leave as Nothing to default to ~/Dropbox/Apps/TaskAgent
customListDirectory :: Maybe FilePath
customListDirectory = Nothing

------- Type definitions -------

type List = [Item]
data Item = Complete   { body :: String }
          | Incomplete { body :: String }
          deriving (Eq, Show)

-------- Read and Show ---------

parseItem :: String -> Maybe Item
parseItem ('-':' ':xs) = Just (Incomplete xs)
parseItem ('x':' ':xs) = Just (Complete xs)
parseItem _            = Nothing

parseList :: String -> Maybe List
parseList = mapM parseItem . lines

ppList :: List -> String
ppList = unlines . zipWith (\n i -> show n ++ " " ++ body i) ([1..]::[Int]) . filter isIncomplete

------------ Logic ------------

checkOffItem :: List -> Int -> Maybe List
checkOffItem list n = case atMay (filter isIncomplete list) n of
                        Nothing -> Nothing
                        Just i  -> Just $ Complete (body i) : delete i list

isIncomplete :: Item -> Bool
isIncomplete (Incomplete _) = True
isIncomplete (Complete   _) = False

------------- IO --------------

main :: IO ()
main = do
  args <- getArgs
  defaultList >>= doesFileExist >>= flip unless (promptToCreate >> exitSuccess)
  case args of
    []             -> listTodos
    ["new"]        -> addTodo
    ["-v"]         -> versionText
    [n]            -> case readMay n :: Maybe Int of
                        Just n' -> completeTodo n'
                        Nothing -> usageText
    _              -> usageText
    
usageText :: IO ()
usageText = do
  prog <- getProgName
  putStr $ unlines [ "Usage: " ++ prog ++ " [new] [NUM]"
                   , ""
                   , prog ++ "     - list items"
                   , prog ++ " new - create new item"
                   , prog ++ " NUM - check-off item NUM"
                   , prog ++ " -v  - show version info"
                   ]

versionText :: IO ()
versionText = putStr $ unlines [ "Todo 0.1 - a TaskAgent-compatible todo list manager"
                               , "(c) 2013 Alex Sayers; licence: BSD 3-Clause." ]

promptToCreate :: IO ()
promptToCreate = do
  path <- defaultList
  response <- query $ "No list found at " ++ path ++ ". Create one? [Yn] "
  unless (response == "n") $ writeFile path ""

listTodos :: IO ()
listTodos = do 
  file <- readFile =<< defaultList
  case parseList file of
    Nothing   -> error "Parse error"
    Just list -> putStr $ ppList list

addTodo :: IO ()
addTodo = do
  todo <- getLine
  path <- defaultList
  appendFile path $ "- " ++ todo ++ "\n"

completeTodo :: Int -> IO ()
completeTodo n = do
  file <- liftM lines $ readFile =<< defaultList
  let target = filter isActive file !! (n-1)
      file' = unlines $ delete target file ++ ['x' : tail target]
  tempName <- withTempFile "todo-" $ \handle ->
    hPutStr handle file'
  removeFile =<< defaultList
  renameFile tempName =<< defaultList
  putStrLn $ 'x' : tail target
  

------- Path resolution --------

listDirectory :: IO FilePath
listDirectory = case customListDirectory of
                         Just path -> return path
                         Nothing   -> defaultListDirectory

defaultListDirectory :: IO FilePath
defaultListDirectory = do
  home <- lookupEnv "HOME"
  case home of
    Just path -> return $ path </> "Dropbox" </> "Apps" </> "TaskAgent"
    Nothing   -> error "Please set list directory manually"

defaultList :: IO FilePath
defaultList = do
  path <- listDirectory
  return $ path </> "todo.txt"

isActive :: String -> Bool
isActive x = head x == '-'

----------- Helpers ------------

-- | Creates a file in the working directory, named by suffixing str with a
-- random number. Its handle is passed to fn. If there is an exception, the
-- file is closed and deleted. Returns the name of the created file.
-- TODO: Perhaps switch to strict IO so dropbox doesn't start synching temporary files
withTempFile :: String -> (Handle -> IO ()) -> IO FilePath
withTempFile str fn = do
  path <- listDirectory
  bracketOnError (openTempFile path str)
                 (\(n,h) -> hClose h >> removeFile n)
                 (\(n,h) -> fn h >> hClose h >> return n)

query :: String -> IO String
query q = do
  putStr q
  hFlush stdout
  getLine
