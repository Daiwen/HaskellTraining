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
  
  
-- |Generic representation of a game state
data GameState a b = Err | GSQuit | 
                     SState { gstate :: a
                            , gsMVar :: MVar (GameInputs b)}


-- |This function retrieves the inputs
getInputs :: (MonadState (GameState a b) m, MonadIO m) => m (GameInputs b)
getInputs = 
  do
    gs <- get
    case gs of      
         SState _ _ -> 
           do
             let iMVar = gsMVar gs
             liftIO $ swapMVar iMVar Unknown
         _                             -> 
           return Quit
           
           
printASCII :: Int -> Char -> [[(Int, Char)]] -> IO()
printASCII gx defaultChar xs =
  putStr $ unlines $ [(' ':replicate gx '_')] ++  
                     (map (printASCII' gx defaultChar gx "") xs) ++
                     [(' ':replicate gx '_')]
                    
  where printASCII' _ _ 0 xs _ = "|" ++ xs ++ "|"
        printASCII' _ dChar idx ys [] =
         "|" ++ replicate idx dChar ++ ys ++ "|"
        printASCII' gx dChar idx ys ((cidx, ch):xs)
          | idx == cidx = 
            printASCII' gx dChar (idx-1) (ch:ys) xs
          | otherwise   = 
            printASCII' gx dChar (idx-1) (dChar:ys) ((cidx, ch):xs)
