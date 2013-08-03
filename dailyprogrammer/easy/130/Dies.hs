module Dies where
 
import Control.Monad
import Control.Monad.Random
import System.Random

   

ndmDices :: (RandomGen g) => (Int, Int) -> Rand g [Int]
ndmDices (n, m) = sequence $ replicate n $ rollDie m

   
   
rollDie :: (RandomGen g) => Int -> Rand g Int
rollDie n = getRandomR (1, n)