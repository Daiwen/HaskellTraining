module Main where
 
import System.Environment
import Control.Monad
import Control.Monad.Random
import ParseNDM
import Dies
import System.Random

 
main :: IO()
main = forever $ do
  dices <- getLine
  s <- liftM unlines $ mapM ((\a -> case a of 
                                Left err ->  return err :: IO String
                                Right val -> liftM show $ evalRandIO $ ndmDices val) 
                             . readNDM) $ words dices
       
  putStr s