module Main where
import Control.Monad
import Control.Monad.State
import SnakeGame
import HDCEngine


    
main :: IO()
main = do 
  --getting an initial state
  igs <- initGameState
  
  evalStateT loop igs
    where
      loop = do
        ns <- liftIO getInputs
--        when (ns /= "quit") $ do
        modify $ (updateGameState) ns        
        fs <- get
        liftIO $ drawGameState fs
        loop    
