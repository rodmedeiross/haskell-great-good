import Control.Applicative (Applicative (liftA2))

sequenceAM :: (Applicative f) => [f a] -> f [a]
sequenceAM = foldr (liftA2 (:)) (pure [])

main :: IO ()
main = do
  print "IO"
