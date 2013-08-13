module HDCEngine where
import Control.Monad.State

class GameInputs a where
  getInputs :: IO a

class GameState a where
  initGameState :: IO a
  updateGameState :: GameInputs b => a -> b -> a
  drawGameState :: a -> IO ()
  
  


  