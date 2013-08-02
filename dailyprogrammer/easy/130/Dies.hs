module Dies where
 
import Control.Monad
import Control.Monad.State
import System.Random


type GeneratorState = State StdGen
   
   
lndmDices :: [Either String (Int, Int)] -> [Either String (GeneratorState [Int])]
lndmDices = map (lndmDices' ndmDices)
  where lndmDices' f x = case x of 
          Left err -> Left err
          Right val -> Right $ f val

ndmDices :: (Int, Int) -> GeneratorState [Int]
ndmDices (0, _) = return []
ndmDices (n, m) = liftM2 (:) (rollDie m) $ ndmDices (n-1, m)
   
   
rollDie :: Int -> GeneratorState Int
rollDie n = do generator <- get
               let (value, newGenerator) = randomR (1, n) generator
               put newGenerator
               return value