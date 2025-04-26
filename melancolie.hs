import Control.Monad
import Data.Ratio

newtype Prob a = Prob {getProb :: [(a, Rational)]} deriving (Show)

instance Functor Prob where
  fmap f (Prob xs) = Prob $ map (\(x, p) -> (f x, p)) xs

instance Applicative Prob where
  pure x = Prob [(x, 1 % 1)]
  (Prob fs) <*> (Prob xs) = Prob [(f x, pf * px) | (f, pf) <- fs, (x, px) <- xs]

thisSituation :: Prob (Prob Char)
thisSituation =
  Prob
    [ (Prob [('a', 1 % 2), ('b', 1 % 2)], 1 % 4),
      (Prob [('c', 1 % 2), ('d', 1 % 2)], 3 % 4)
    ]

-- Prob $ concat  $ map multAll xs
-- where multAll (Prob innerxs p) = map (\(x, r) -> (x, p * r)) innerxs

flatten :: Prob (Prob a) -> Prob a
flatten (Prob xs) = Prob $ concatMap multAll xs
  where
    multAll (Prob innerxs, p) = map (\(a, i) -> (a, p * i)) innerxs

instance Monad Prob where
  return = pure
  m >>= f = flatten (fmap f m)

main :: IO ()
main = undefined
