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
          deriving (Eq)

instance Show Item where
  show (Complete str) = "x " ++ str
  show (Incomplete str) = "- " ++ str

------------ Logic ------------

usageText :: String -> String
usageText name = unlines [ "Usage: " ++ name ++ " [new] [NUM]"
                         , ""
                         , name ++ "     - list items"
                         , name ++ " new - create new item"
                         , name ++ " NUM - check-off item NUM"
                         , name ++ " -v  - show version info"
                         ]

versionText :: String
versionText = unlines [ "Todo 0.1 - a TaskAgent-compatible todo list manager"
                      , "(c) 2013 Alex Sayers; licence: BSD 3-Clause." ]

parseItem :: String -> Maybe Item
parseItem ('-':' ':xs) = Just (Incomplete xs)
parseItem ('x':' ':xs) = Just (Complete xs)
parseItem _            = Nothing

-- TODO: Ignore empty lines
parseList :: String -> Maybe List
parseList = mapM parseItem . lines

numberList :: List -> String
numberList = unlines . zipWith (\n i -> show n ++ " " ++ body i) ([1..]::[Int]) . filter isIncomplete

serialiseList :: List -> String
serialiseList = unlines . map show

checkOffItem :: List -> Int -> Maybe List
checkOffItem list n = liftM (\i -> Complete (body i) : delete i list) (atMay (filter isIncomplete list) n)

isIncomplete :: Item -> Bool
isIncomplete (Incomplete _) = True
isIncomplete (Complete   _) = False

------------- IO --------------

main :: IO ()
main = do
  args <- getArgs
  defaultList >>= doesFileExist >>= flip unless (promptToCreate >> exitSuccess)
  case args of
    []             -> putStr . numberList =<< loadDefaultList
    ["new"]        -> addTodo
    ["-v"]         -> putStr versionText
    [n]            -> case readMay n :: Maybe Int of
                        Just n' -> completeTodo n'
                        Nothing -> putStr . usageText =<< getProgName
    _              -> putStr . usageText =<< getProgName
    
promptToCreate :: IO ()
promptToCreate = do
  path <- defaultList
  response <- query $ "No list found at " ++ path ++ ". Create one? [Yn] "
  unless (response == "n") $ writeFile path ""

loadDefaultList :: IO List
loadDefaultList = do
  file <- readFile =<< defaultList
  case parseList file of
    Nothing   -> error "Couldn't parse list"
    Just list -> return list

addTodo :: IO ()
addTodo = do
  todo <- getLine
  path <- defaultList
  appendFile path $ show (Incomplete todo) ++ "\n"

-- TODO: Perhaps switch to strict IO so dropbox doesn't start synching temporary files
-- TODO: Print item which was checked off
completeTodo :: Int -> IO ()
completeTodo n = do
  list <- loadDefaultList
  case checkOffItem list (n-1) of
    Nothing    -> error "Item not found"
    Just list' -> do
      tempName <- withTempFile "todo-" $ \handle ->
        hPutStr handle $ serialiseList list'
      removeFile =<< defaultList
      renameFile tempName =<< defaultList

------- Path resolution --------

defaultList :: IO FilePath
defaultList = do
  path <- listDirectory
  return $ path </> "todo.txt"

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

----------- Helpers ------------

-- | Creates a file in the working directory, named by suffixing str with a
-- random number. Its handle is passed to fn. If there is an exception, the
-- file is closed and deleted. Returns the name of the created file.
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
