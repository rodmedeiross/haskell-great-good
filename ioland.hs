module Main where

import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.IO

reverseWords :: String -> String
reverseWords = unwords . map reverse . words

filterContents :: String -> String
filterContents = unlines . filter (\x -> length x < 10) . lines

isPall :: String -> Bool
isPall xs = reverse xs == xs

respondIsPall :: String -> String
respondIsPall = unlines . map (\x -> if isPall x then "Palandrome" else "Not Palandrome") . lines

type FilePath' = String

withFile' :: FilePath' -> IOMode -> (Handle -> IO a) -> IO a
withFile' path mode f =
  bracket
    (openFile path mode)
    (\handler -> hClose handler)
    (\handler -> f handler)

-- main = do
--   readed <- getLine
--   unless (null readed) $ do
--     putStrLn $ "Lets reverse: " ++ reverseWords readed
--     main

-- main = do
--   colors <-
--     forM
--       [1, 2, 3, 4]
--       ( \a -> do
--           putStrLn $ "Which is the color associate with " ++ show a ++ " ?"
--           getLine
--       )
--   putStrLn "The assosciates with 1, 2, 3 and 4 are: "
--   putStrLn $ unwords colors

-- main = forever $ do
--   l <- getLine
--   putStrLn $ map toUpper l

-- main = do
--   l <- getContents
--   putStr $ map toUpper l

-- main = do
--   ct <- getContents
--   putStr $ filterContents ct

-- main = interact filterContents
-- main = interact respondIsPall

-- main = do
--   handler <- openFile "haiku.txt" ReadMode
--   contents <- hGetContents handler
--   putStr contents
--   hClose handler
--
--   withFile
--     "haiku.txt"
--     ReadMode
--     ( \handler -> do
--         contents <- hGetContents handler
--         putStr contents
--     )
--
--   appendFile "haiku.txt" ("TESTE" ++ "\n")

main = do
  linesC <- getLine
  appendFile "haiku.txt" (linesC ++ "\n")

  contents <- readFile "haiku.txt"

  let todoList = lines contents
      numberedList = zipWith (\n l -> show n ++ " - " ++ l) [0 ..] todoList

  putStrLn "-- Look at the TO-DO list: --"

  mapM_ putStrLn numberedList

  putStrLn "Which one want to delete?"
  numberStr <- getLine

  let number = read numberStr
      newTodoList = unlines $ delete (todoList !! number) todoList

  (tempName, tempHandler) <- openTempFile "." "temp"
  hPutStr tempHandler newTodoList
  hClose tempHandler
  removeFile "haiku.txt"
  renameFile tempName "haiku.txt"
