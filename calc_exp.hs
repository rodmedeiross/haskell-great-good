import Control.Monad
import System.IO

resolveCalcRPN :: String -> Double
resolveCalcRPN = head . foldl evaluate [] . words
  where
    evaluate (x : y : xs) "+" = (y + x) : xs
    evaluate (x : y : xs) "*" = (y * x) : xs
    evaluate (x : y : xs) "-" = (y - x) : xs
    evaluate (x : y : xs) "/" = (y / x) : xs
    evaluate (x : y : xs) "^" = (y ** x) : xs
    evaluate (x : xs) "log" = log x : xs
    evaluate acc "sum" = [sum acc]
    evaluate acc number = read number : acc

main :: IO ()
main = do
  let eval = "10 4 3 + 2 * -"
   in putStrLn $ "Result of " ++ show eval ++ " is that " ++ show (resolveCalcRPN eval)

  let eval = "10 10 10 10 sum 4 /"
   in putStrLn $ "Result of " ++ show eval ++ " is that " ++ show (resolveCalcRPN eval)

  let eval = "2.7 log"
   in putStrLn $ "Result of " ++ show eval ++ " is that " ++ show (resolveCalcRPN eval)
