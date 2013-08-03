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
  s <- myUnlines $ map (\a -> case a of 
                           Left err ->  Left err 
                           Right val -> Right $ liftM show $ evalRandIO $ ndmDices val) 
                       $ map readNDM $ words dices
  putStr s
  
myUnlines :: [Either String (IO String)] -> IO String
myUnlines [] = return []
myUnlines ((Left a):xs) = liftM2 (++) (return $ a ++ "\n") $ myUnlines xs
myUnlines ((Right a):xs) = liftM2 (\x y ->x ++ "\n" ++ y) a $ myUnlines xs