module Main where
import Control.Monad
import Control.Monad.State
import SnakeGame
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
        fs <- get
        drawGameState
        
        ns <- getInputs 
        
        when (ns /= Quit) $ do
          updateGameState ns
          loop
        

          
        