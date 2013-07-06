module Main where

import System.Environment (getArgs, getProgName, lookupEnv)
import Control.Exception (bracketOnError)
import System.IO (openTempFile, Handle, hClose, hPutStr)
import System.Directory (removeFile, renameFile, doesFileExist)
import Data.List (delete)
import Control.Monad (liftM, unless)
import System.FilePath ((</>))
import Safe (readMay)


------------ Config ------------

-- | Change this to Just a filepath to specify the location of your todo list
-- files. Leave as Nothing to default to ~/Dropbox/Apps/TaskAgent
customListDirectory :: Maybe FilePath
customListDirectory = Nothing

---------- End config ----------

-- TODO: Almost everything is in the IO monad. See if a judicious refactor can change that.

-- TODO: check for list existence. Query to create if not present.
main :: IO ()
main = do
  args <- getArgs
  defaultList >>= doesFileExist >>= flip unless promptToCreate
  case args of
    []             -> listTodos
    ["new"]        -> addTodo
    ["-v"]         -> putStrLn "Todo 0.1 - a TaskAgent-compatible todo list manager\n(c) 2013 Alex Sayers; licence: BSD 3-Clause."
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
                   ]

promptToCreate :: IO ()
promptToCreate = error "Uh oh, can't find the default list!"

listTodos :: IO ()
listTodos = putStr . unlines . number . map (drop 2) . filter isActive . lines =<< readFile =<< defaultList
  where number = zipWith (\x y -> show x ++ " " ++ y) ([1..]::[Int])

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

-- TODO: be aware of multiple lists.
defaultList :: IO FilePath
defaultList = do
  path <- listDirectory
  return $ path </> "todo.txt"

isActive :: String -> Bool
isActive x = head x == '-'

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
