module Main where

import System.Environment (getArgs, getProgName, lookupEnv)
import Control.Exception (bracketOnError)
import System.IO (openTempFile, Handle, hClose, hPutStr)
import System.Directory (removeFile, renameFile)
import Data.List (delete)
import Control.Monad (liftM)
import System.FilePath ((</>))

-- | You can change this to specify the location of your todo list files
customListDirectory :: Maybe FilePath
customListDirectory = Nothing

main :: IO ()
main = do
  args <- getArgs
  case args of
    []             -> listTodos
    ["new"]        -> addTodo
    [n]            -> bracketOnError (return (read n :: Int)) (const usageText) deleteTodo
    _              -> usageText
    
listTodos :: IO ()
listTodos = putStr . unlines . number . map (drop 2) . filter isActive . lines =<< readFile =<< defaultList
  where number = zipWith (\x y -> show x ++ " " ++ y) ([1..]::[Int])

addTodo :: IO ()
addTodo = do
  todo <- getLine
  path <- defaultList
  appendFile path $ "- " ++ todo ++ "\n"

deleteTodo :: Int -> IO ()
deleteTodo n = do
  file <- liftM lines $ readFile =<< defaultList
  let target = filter isActive file !! (n-1)
      file' = unlines $ delete target file ++ ['x' : tail target]
  tempName <- withTempFile "todo-" $ \handle ->
    hPutStr handle file'
  removeFile =<< defaultList
  renameFile tempName =<< defaultList
  
usageText :: IO ()
usageText = do
  prog <- getProgName
  putStrLn $ unlines [ "Usage: " ++ prog ++ " [new] [NUM]"
                     , ""
                     , prog ++ "     - list todos"
                     , prog ++ " new - create new todo"
                     , prog ++ " NUM - check-off todo NUM"
                     ]

-- | Attempt to guess list directory
defaultListDirectory :: IO FilePath
defaultListDirectory = case customListDirectory of
                         Just path -> return path
                         Nothing   -> do
                           home <- lookupEnv "HOME"
                           case home of
                             Just path -> return $ path </> "Dropbox" </> "Apps" </> "TaskAgent"
                             Nothing   -> error "Please set list directory manually"

defaultList :: IO FilePath
defaultList = do
  path <- defaultListDirectory
  return $ path </> "todo.txt"

isActive :: String -> Bool
isActive x = head x == '-'

-- | Creates a file in the working directory, named by suffixing str with a
-- random number. Its handle is passed to fn. If there is an exception, the
-- file is closed and deleted. Returns the name of the created file.
withTempFile :: String -> (Handle -> IO ()) -> IO FilePath
withTempFile str fn = do
  path <- defaultListDirectory
  bracketOnError (openTempFile path str)
                 (\(n,h) -> hClose h >> removeFile n)
                 (\(n,h) -> fn h >> hClose h >> return n)
