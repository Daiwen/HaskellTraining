module HDCEngine where


import Control.Concurrent
import Control.Monad.State


-- |Generic input representation 
data GameInputs a = Quit | Unknown | SInputs a 




--Listener function updating the shared variable as new inputs 
--are entered.
getInputsThread :: MVar (GameInputs a) -> (Char -> GameInputs a) -> IO ()
getInputsThread iMVar readInputs = do
  x <- getChar
  swapMVar iMVar $ readInputs x
  getInputsThread iMVar readInputs