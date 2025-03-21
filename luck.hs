import Control.Monad (when)
import System.IO
import System.Random

main = do
  gen <- getStdGen
  getBets gen

getBets :: StdGen -> IO ()
getBets gen = do
  let (rand, newGen) = randomR (1, 10) gen :: (Int, StdGen)

  putStrLn "Escolha entre um numero de 1 a 10"
  line <- getLine

  when (not $ null line) $ do
    let number = read line

    if number == rand
      then putStrLn "Congrats and Jump in the fire"
      else putStrLn $ "Sorry, u are wrong, it is " ++ show rand
    getBets newGen
