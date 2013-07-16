module Main where

import Control.Monad.State
import System.IO
import System.Environment

main :: IO()
main = do
  args <- getArgs
  putStr $ evalState (mc91 $ read $ head args) MC91Datatype 
                    {cString = "M(" ++ head args ++ ")\n",
                     cFront = "M(",
                     cBack = ")",
                     cValue = 0}
  
data MC91Datatype = MC91Datatype
                  {cString :: String,
                   cFront :: String,
                   cBack :: String,
                   cValue :: Int}

      
mc91 :: Int -> State MC91Datatype String
mc91 n = do
  a <- get
  let b = cFront a;
      c = cBack a
  if n > 100
    then do
      let cs = cString a ++ (init . init) b ++ 
                 show (n-10) ++ tail c ++ " since " ++ show n ++ " > 100\n"
      put MC91Datatype {cString = cs,
                        cFront = (init . init) b,
                        cBack = tail c,
                        cValue = n-10}
      return cs
    else do
      let st = MC91Datatype {cString = cString a ++ b ++ "M(" ++ 
                                       show (n + 11) ++ ")" ++ c ++ 
                                       " since " ++ show n ++ " <= 100\n",
                             cFront =  b ++ "M(",
                             cBack = ")" ++ c,
                             cValue = n + 11}
          nst = execState (mc91 $ n+11) st
          nnst = execState (mc91 $ cValue nst) nst
      put nnst
      return $ cString nnst
         