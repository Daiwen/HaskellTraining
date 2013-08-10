module Main where
import Control.Monad
import System.IO
import Data.List

import Control.Monad
import Control.Monad.Trans
import Control.Monad.Trans.State.Strict

-- Classic
-- main :: IO ()
-- main = do
--   putStrLn "This program saves what you say and repeats it:"  
--   loop [] 
       
-- loop xs = do 
--   putStr "> "
--   hFlush stdout
--   x <- getLine 
--   let zs = xs++x 
--   putStrLn zs
--   loop zs

-- Point free
-- main :: IO()
-- main = interact (unlines . map unwords . tail . inits . lines)

-- StateT introduction
-- main :: IO()
-- main = do 
--   putStrLn "This program saves what you say and repeats it:"  
--   evalStateT loop [] 
--     where
--       loop = do
--         string <- get
--         liftIO $ putStr "> "
--         liftIO $ hFlush stdout
--         nstring <- liftIO getLine
--         when (nstring /= "quit") $
--           do let acc = string ++ nstring
--              liftIO $ putStrLn acc
--              put acc
--              loop
             
-- StateT cleaning
main :: IO()
main = do 
  putStrLn "This program saves what you say and repeats it:"  
  evalStateT loop [] 
    where
      loop = do
        liftIO $ putStr "> "
        liftIO $ hFlush stdout
        nstring <- liftIO getLine
        when (nstring /= "quit") $
          do modify (++ nstring)
             s <- get
             liftIO $ putStrLn s
             loop