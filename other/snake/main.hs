module Main where
import Control.Monad
import Control.Monad.State
import SnakeGame
import HDCEngine
import System.IO
import Control.Concurrent
import System.Random


    
main :: IO()
main = do 
  --getting an initial state
  hSetBuffering stdin NoBuffering
  igs <- initGameState

  evalStateT loop igs
    where
      loop = do
        drawGameState
        
        ns <- getInputs 
        
        when ((\x -> case x of 
                  Quit -> False
                  _    -> True ) ns) $ do
          updateGameState ns
          loop
        

          
