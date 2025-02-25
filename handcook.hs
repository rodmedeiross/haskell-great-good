module Main where

import Data.Char
import Data.List
import Data.Map qualified as Map
import Distribution.Compat.Lens (_1)

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x : xs) = f x : map' f xs

collatz' :: Int -> [Int]
collatz' 1 = [1]
collatz' x
  | even x = x : collatz' (div x 2)
  | odd x = x : collatz' ((*) x 3 + 1)

reverse' :: [a] -> [a]
reverse' = foldl (flip (:)) []

product' :: (Num a) => [a] -> a
product' = foldl (*) 1

sumSqrt :: Int
sumSqrt = length . takeWhile (< 10000) . scanl1 (+) $ map sqrt [1 ..]

and' :: [Bool] -> Bool
and' = foldr (&&) True

fn = ceiling . negate . tan . cos . max 50

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

isThere :: (Eq a) => [a] -> [a] -> Bool
needle `isThere` haystack = any (needle `isPrefixOf`) (tails haystack)

ceasarCypher :: Int -> String -> String
ceasarCypher magik = map (chr . (+ magik) . ord)

sumDigits :: Int -> Int
sumDigits = sum . map digitToInt . show

findDigits :: Int -> Maybe Int
findDigits n = find (\x -> sumDigits x == n) [1 ..]

infixr 5 :-:

data List a = Nil | a :-: (List a) deriving (Show)

data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving (Show, Eq)

singletonTree :: a -> Tree a
singletonTree x = Node x EmptyNode EmptyNode

insertTree :: (Ord a) => a -> Tree a -> Tree a
insertTree x EmptyNode = singletonTree x
insertTree x (Node a left right)
  | x == a = Node a left right
  | x < a = Node a (insertTree x left) right
  | x > a = Node a left (insertTree x right)

elemTree :: (Ord a) => a -> Tree a -> Bool
elemTree x EmptyNode = False
elemTree x (Node a left right)
  | x == a = True
  | x < a = elemTree x left
  | x > a = elemTree x right

-- instance (Eq c) => Eq (Maybe c) where
--   Just x == Just y = x == y
--   Nothing == Nothing = True
--   _ == _ = False

class If a where
  ifthis :: a -> Bool

instance If Int where
  ifthis 0 = False
  ifthis _ = True

instance If [a] where
  ifthis [] = False
  ifthis _ = True

instance If Bool where
  ifthis = id

instance If (Maybe a) where
  ifthis (Just _) = True
  ifthis Nothing = False

instance If (Tree a) where
  ifthis EmptyNode = False
  ifthis _ = True

ifThis :: (If b) => b -> a -> a -> a
ifThis c i e =
  if ifthis c
    then i
    else e

instance Functor Tree where
  fmap f EmptyNode = EmptyNode
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)

lockerLookUp :: Int -> LockerMap -> Either String Code
lockerLookUp n map = case Map.lookup n map of
  Nothing -> Left $ "Bla bla bla" ++ show n ++ "Bla bla bla"
  Just (state, code) ->
    if state /= Taken
      then Right code
      else Left "Bla bla bla"

-- fmap Either a => (b -> c) -> Either a b -> Either a c

main :: IO ()
main = do
  let list1 = [1, 2, 4, 5]
      list2 = [5, 2, 4, 5]
   in print $ zipWith' (*) list1 list2

  let list3 = [1, 3, 4, 5, 3]
   in print $ map' (* 2) list3

  let len = length $ filter (\xs -> length xs > 15) $ map collatz' [1 .. 100]
   in print len

  let cyper = "Common Baby"
   in print $ ceasarCypher 3 cyper

  let number = 40
   in print $ findDigits number

  let numbers = [4, 3, 5, 6, 2, 3, 5, 4, 2, 6, 7, 8, 2, 4, 11, 23, 12, 54, 54, 23, 42, 12, 43, 53, 65, 76, 3, 7, 24, 76]
      tree = foldl (flip insertTree) EmptyNode numbers
   in print $ fmap (* 2) tree
