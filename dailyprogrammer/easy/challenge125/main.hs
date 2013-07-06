module Main where

import Control.Monad.State
import System.IO
import System.Environment
import Data.Char
import WCData

main :: IO()
main = do
  args <- getArgs
  hfile <- openFile (head args) ReadMode
  sfile <- hGetContents hfile
  putStr $ analyze $ map toLower sfile

analyze :: String -> String
analyze a = show $ evalState (process a) 
            ([], Analytics {wordNumber = 0,
                       letterNumber = 0,
                       symbolNumber = 0,
                       wordList = [],
                       letterList = []})

type ProcessState = (String, Analytics)

data CharType = WS | LE | SY | UN
  
whatIs :: Char -> CharType
whatIs x 
  | isSpace x = WS
  | isAlphaNum x = LE
  | otherwise = SY

                              
                          
process :: String -> State ProcessState Analytics
process [] = do
  (_, a) <- get
  return a
process (x:xs) = do
  (cs, a) <- get
  case whatIs x of
    WS -> if null cs || (isSpace $ head cs)
          then put ([x], a)
          else put ([x], addWord a $ reverse cs)
    LE -> if not (null cs) && (isSpace $ head cs)
          then put ((x:[]), addLetter a x)
          else put ((x:cs), addLetter a x)
    SY -> if null cs || (isSpace $ head cs)
          then put ([], addSymbol a x)
          else put ([], addWord (addSymbol a x) $ reverse cs)
  process xs
    
    
      
  