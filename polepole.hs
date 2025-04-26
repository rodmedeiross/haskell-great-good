import Control.Monad

type Birds = Int

type Pole = (Birds, Birds)

appendRight :: Birds -> Pole -> Either String Pole
appendRight b (l, r)
  | abs (r + b) - l < 4 = Right (l, r + b)
  | otherwise = Left $ "U felt - Left has " ++ show l ++ " and Right has " ++ show (r + b)

appendLeft :: Birds -> Pole -> Either String Pole
appendLeft b (l, r)
  | abs r - (l + r) < 4 = Right (l + b, r)
  | otherwise = Left $ "U felt - Left has " ++ show (l + b) ++ " and Right has " ++ show r

main :: IO ()
main = do
  let res = appendRight 1 (0, 0) >>= appendRight 2 >>= appendLeft 3 >>= appendLeft 1 >>= appendRight 4
   in print res
