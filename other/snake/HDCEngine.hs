module HDCEngine where
import Control.Monad.State

class GameState a where
  initGameState :: IO a
  nextGameState :: IO a
  updateGameState :: a -> a -> a
  drawGameState :: a -> IO ()
  
  


  