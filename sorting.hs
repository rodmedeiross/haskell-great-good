module Main where

mergeSort :: (Ord a, Num a) => [a] -> [a]
mergeSort [] = []
mergeSort (x : xs) =
  let sOrE = [a | a <- xs, a <= x]
      gOf = [b | b <- xs, b > x]
   in mergeSort sOrE ++ [x] ++ mergeSort gOf

main :: IO ()
main = do
  print $ mergeSort [5, 1, 9, 10, 6, 7, 3]
