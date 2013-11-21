module Main where
import Control.Monad
import Control.Monad.State
import SnakeGame
import System.IO
import Control.Concurrent


    
main :: IO()
main = do 
  --getting an initial state
  hSetBuffering stdin NoBuffering
  igs <- initGameState
  
  iMVar <- newMVar $ SnakeInputs SnDown  
  forkIO (getInputs iMVar)

  evalStateT (loop iMVar) igs
    where
      loop iMVar = do
        fs <- get
        liftIO $ drawGameState fs
        liftIO $ threadDelay 100000
        ns <- liftIO $ swapMVar iMVar Unknown
        when (ns /= Quit) $ do
          modify $ flip updateGameState ns
          loop iMVar
        

          
        