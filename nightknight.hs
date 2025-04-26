import Control.Monad
import Data.List (find)

type KnightPos = (Int, Int)

walkKnight :: KnightPos -> [KnightPos]
walkKnight (r, c) =  do
  (r', c') <- [(r-2, c-1), (r-2, c+1), (r-1, c+2), (r+1, c+2), (r+2, c+1), (r+2, c-1), (r+1, r-2), (r-1, c-2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (r', c')

roundsWalking :: Int -> KnightPos -> [KnightPos]
roundsWalking 0 pos = [pos]
roundsWalking n pos = walkKnight pos >>= roundsWalking (n -1)

roundsWalking' :: Int -> KnightPos -> [KnightPos]
roundsWalking' n pos = return start >>= foldr (<=<) return (replicate x walkKnight)

roundsWalking n pos = do 
  next <- walkKnight pos
  roundsWalking (n-1) next


type KnightToResult = Maybe (Int, [KnightPos])
walkFromTo :: KnightPos -> KnightPos -> KnightToResult
walkFromTo from to = find (\(_, path) -> last path == to)(bfs [[from]] 0)
  where
    bfs [] _ = []
    bfs paths step =
      let newPaths = [path ++ [next] | path <- paths, next <- walkKnight (last path), next `notElem` path]
      in map (\p -> (step + 1, p)) newPaths ++ bfs newPaths (step +1)
  


main:: IO()
main = do
  let res = walkKnight (6,4)
      rounded = roundsWalking 3 (6,4)
      goTo = walkFromTo (6,4) (5,8)
    in print $ "One Movement: " ++ show res ++ "| Possibilities: " ++ show rounded ++ "| Shortest Straw: " ++ show goTo


-- instance Monad [] where
--  return x = [x]
--    xs >>= f = concat (map f xs)
--    fail _ = []
