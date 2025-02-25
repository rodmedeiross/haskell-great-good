import Control.Exception
import Control.Monad
import Data.Char
import Data.List
import System.Directory
import System.Environment
import System.IO

dispatcher :: String -> [String] -> IO ()
dispatcher "add" = addItem
dispatcher "show" = showItem
dispatcher "delete" = deleteItem

addItem :: [String] -> IO ()
addItem [] = return ()
addItem [fileName] = return ()
addItem (filename : one : args) = do
  appendFile filename (one ++ "\n")
  addItem (filename : args)

deleteItem :: [String] -> IO ()
deleteItem [] = return ()
deleteItem (filename : one : rest) = do
  showItem [filename]
  content <- readFile filename

  let idx = read one
      todoList = lines content
      newContent = unlines $ delete (todoList !! idx) todoList

  bracketOnError
    (openTempFile "." "temp")
    ( \(tempName, tempHandler) -> do
        hClose tempHandler
        removeFile tempName
    )
    ( \(tempName, tempHandler) -> do
        hPutStr tempHandler newContent
        hClose tempHandler
        removeFile filename
        renameFile tempName filename
    )

showItem :: [String] -> IO ()
showItem [] = return ()
showItem [filename] = do
  content <- readFile filename
  let numbered = zipWith (\n i -> show n ++ " - " ++ i) [0 ..] (lines content)
  putStrLn "My TODOs:"
  mapM_ putStrLn numbered
  putStr "\r\n"

main :: IO ()
main = forever $ do
  args <- getArgs

  if length args < 0
    then dispatcher (head args) (tail args)
    else do
      putStrLn "Wellcome TO...DO list"
      putStrLn "0 - show"
      putStrLn "1 - add"
      putStrLn "2 - delete"
      putStrLn "Pattern: Command File Args"
      putStr "\r\n"

      input <- getLine

      case words input of
        (n : arg) -> case read n of
          0 -> dispatcher "show" [head arg]
          1 -> dispatcher "add" arg
          2 -> dispatcher "delete" arg
          _ -> putStrLn "Invalid Command"
        _ -> putStrLn "Command Pattern Wrong"
