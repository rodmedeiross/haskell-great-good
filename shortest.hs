module Main where

import Control.Monad
import Data.List
import System.IO

data Section = Section {getA :: Int, getB :: Int, getC :: Int} deriving (Show)

type RoadSystem = [Section]

heathrowToLodon :: RoadSystem
heathrowToLodon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
  let actualTimeA = sum $ map snd pathA
      actualTimeB = sum $ map snd pathB

      forwardFromA = actualTimeA + a
      forwardFromB = actualTimeB + b

      crossToA = actualTimeB + b + c
      crossToB = actualTimeA + a + c

      newPathToA =
        if forwardFromA <= crossToA
          then (A, a) : pathA
          else (C, c) : (B, b) : pathB
      newPathToB =
        if forwardFromB <= crossToB
          then (B, b) : pathB
          else (C, c) : (A, a) : pathB
   in (newPathToA, newPathToB)

bestPath :: RoadSystem -> Path
bestPath roadSystem =
  let (bestA, bestB) = foldl roadStep ([], []) roadSystem
   in if sum (map snd bestA) <= sum (map snd bestB)
        then reverse bestA
        else reverse bestB

groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf x xs = take x xs : groupsOf x (drop x xs)

main :: IO ()
main = do
  contents <- getContents
  let groups = groupsOf 3 (map read $ lines contents)
      roadSystem = map (\[a, b, c] -> Section a b c) groups
      path = bestPath roadSystem
      pathString = concatMap (show . fst) path
      bestTime = sum (map snd path)

  putStrLn $ "The Best path to take is: " ++ pathString
  putStrLn $ "Time taken: " ++ show bestTime

  let result = bestPath heathrowToLodon
   in print result
