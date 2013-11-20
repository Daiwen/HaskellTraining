module Main where
import Control.Monad
import Control.Monad.State
import SnakeGame
import System.IO



    
main :: IO()
main = do 
  --getting an initial state
  hSetBuffering stdin NoBuffering
  igs <- initGameState
  
  evalStateT loop igs
    where
      loop = do
        fs <- get
        liftIO $ drawGameState fs
        ns <- liftIO getInputs
--        when (ns /= "quit") $ do
        modify $ flip updateGameState ns
        loop    
