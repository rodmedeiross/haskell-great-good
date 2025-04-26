import Control.Monad
import Control.Monad.Writer

newtype DiffList a = DiffList {getDiffList :: [a] -> [a]}

instance Semigroup (DiffList a) where
  (<>) (DiffList f) (DiffList g) = DiffList $ f . g

instance Monoid (DiffList a) where
  mempty = DiffList id
  mappend = (<>)

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs ++)

getNormalList :: DiffList a -> [a]
getNormalList (DiffList f) = f []

finalCountDown :: Int -> Writer (DiffList String) ()
finalCountDown 0 = do
  tell $ toDiffList ["0"]
finalCountDown x = do
  finalCountDown (x - 1)
  tell $ toDiffList [show x]

finalCountDown' :: Int -> Writer [String] ()
finalCountDown' 0 = do
  tell ["0"]
finalCountDown' x = do
  finalCountDown' (x - 1)
  tell [show x]

main :: IO ()
main = do
  let res = snd . runWriter $ finalCountDown' 5000
   in print res
