module Main where
 
import System.Environment
import Control.Monad
import Control.Monad.State
import ParseNDM
import Dies
import System.Random

 
main :: IO()
main = forever $ do
  r <- randomRIO (1, 1000)
  dices <- getLine
  putStr $ unlines $ map (\a -> case a of 
                             Left err -> err          
                             Right val -> show $ evalState val (mkStdGen r)) 
                         $ lndmDices $ map readNDM $ words dices
