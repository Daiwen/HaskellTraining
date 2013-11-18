module HDCEngine where
import Control.Monad.State

class GameInputs a where
  getInputs :: IO a

class GameInputs b => GameState a b where
  initGameState :: IO a
  updateGameState :: a -> b -> a
  drawGameState :: a -> IO ()
  
  


  