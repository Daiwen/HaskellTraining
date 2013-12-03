module SnakeGame where

import System.IO
import System.Random
import System.Environment
import Data.List
import Control.Concurrent
import Control.Monad.State as S
import Text.ParserCombinators.Parsec
import Text.Parsec.Char
import Text.Parsec.Prim
import Text.Printf


import HDCEngine


-- |Represents the different recognised inputs for the snake game.
data SnakeInputs = SnUp | SnDown | SnLeft | SnRight deriving (Eq)


    
--Convert a character to the corresponding GameInputs
readInputs :: Char -> GameInputs SnakeInputs
readInputs x  
  | x == 'z'    = SInputs SnUp
  | x == 's'    = SInputs SnDown
  | x == 'q'    = SInputs SnLeft
  | x == 'd'    = SInputs SnRight
  | x == '\ESC' = Quit
  | otherwise   = Unknown

-- |Represents the size of the snake grid
type GridSize = (Int, Int)

-- |Represents a position on the grid
type Position = (Int, Int)

-- |The snake recursive representation
data Snake = SnTail | Head { direction :: SnakeInputs
                           , size      :: Int
                           , sntail    :: Snake
                           }

--Update the player position and state according to the inputs
updatePlayer :: MonadState (GameState SnakeGame SnakeInputs) m =>
                GameInputs SnakeInputs -> m (Position, Snake)
updatePlayer si = 
  do
    npos <- updateSnakePosition si
    nsnk <- updateSnake         si
    return (npos, nsnk) 


--Update the snake position
updateSnakePosition :: MonadState (GameState SnakeGame SnakeInputs) m => 
                       GameInputs SnakeInputs -> m Position
updateSnakePosition si =
  do 
    ggs <- get     
    let gs = gstate ggs
        (spos, snk) = snake gs
        sndir       = 
            case si of 
              Unknown -> SInputs $ direction snk               
              _       -> si

    return $ updateSnakePosition' (grid gs) spos sndir
  where 
    updateSnakePosition' (gx, gy) (a, b) si =
      case si of
        (SInputs    SnUp) -> (a,              (b-1) `mod` gy)
        (SInputs  SnDown) -> (a,              (b+1) `mod` gy)
        (SInputs  SnLeft) -> ((a-1) `mod` gx,  b            )
        (SInputs SnRight) -> ((a+1) `mod` gx,  b            )



--Update the snake representation
updateSnake :: MonadState (GameState SnakeGame SnakeInputs) m => 
               GameInputs SnakeInputs -> m Snake
updateSnake Unknown = 
  do
    ggs <- get
    let gs = gstate ggs
        (_, snk) = snake gs
    updateSnake $ SInputs $ direction snk
updateSnake (SInputs i) =
  do
    ggs <- get
    let gs = gstate ggs
        (ps, sn)        = snake gs
        (pf, _ )        = food  gs
        (nsze, nsntail) = 
          case (ps == pf, direction sn == i, sntail sn) of
            (True,  True , SnTail) -> (1 + size sn, SnTail   )   
            (True,  True , _     ) -> (1 + size sn, sntail sn)
            (True,  False, _     ) -> (1, sn)
            (False, True , SnTail) -> (size sn, SnTail)   
            (False, False, SnTail) -> (1, Head (direction sn) 
                                               (size  sn - 1) 
                                               SnTail        )
            (False, True , _     ) -> (1 + size sn, shortenTail $ sntail sn)
            (False, False, _     ) -> (1, shortenTail sn)
    return Head {direction = i,
                 size = nsze,
                 sntail = nsntail}
  where 
    --Reduces the size of the snake tail by 1
    shortenTail s = 
      case sntail s of
        Head hsnk sze hsntail -> Head (direction            s)
                                      (size                 s) 
                                      (shortenTail $ sntail s)
        SnTail                -> if size s > 1
                                 then Head (direction s)
                                           (size  s - 1)
                                           (SnTail)
                                 else SnTail

                         
-- |Predicate stating wether or not the snake eats itself.          
isLostSnake :: Snake -> Bool
isLostSnake snk = isLostSnake' nsnk vcpt hcpt
  where nsnk | size snk <= 1 = sntail snk
             | otherwise     = Head (direction snk) 
                                    (size snk -  1) 
                                    (sntail    snk)

        (vcpt, hcpt) = 
          case direction snk of
            SnRight -> ( 0,  1)
            SnLeft  -> ( 0, -1)
            SnUp    -> ( 1,  0)
            SnDown  -> (-1,  0)
            
        isLostSnake' _ 0 0 = True
        isLostSnake' SnTail _ _ = False
        isLostSnake' (Head dir sze stail) vcpt hcpt =
          isLostSnake' nsnk (vcpt+v) (hcpt+h) ||
          isLostSnake' nsnk (0+v) (0+h)
          where nsnk | sze <= 1  = stail
                     | otherwise = Head dir (sze-1) stail
                      
                (v,h) = 
                  case dir of
                    SnRight -> ( 0,  1)
                    SnLeft  -> ( 0, -1)
                    SnUp    -> ( 1,  0)
                    SnDown  -> (-1,  0)




-- |Represents the food element in the game
data Food = Food

-- Updates the foog position and representation
updateFood :: MonadState (GameState SnakeGame SnakeInputs) m => 
              m (Position, Food)
updateFood = 
  do 
    gs <- get 
    let (_, f) = food $ gstate gs
    fpos <- updateFoodPosition
    return (fpos, f)

-- Updates food position depending on the player position
updateFoodPosition :: MonadState (GameState SnakeGame SnakeInputs) m => 
                      m Position
updateFoodPosition = 
  do 
    ggs <- get
    let gs = gstate ggs 
        (fpos,  _) = food gs
        (spos,  _) = snake gs        
        (gx  , gy)  = grid gs
        (rpos, nseed) = runState (do x <- state $ randomR (0, gx-1)
                                     y <- state $ randomR (0, gy-1)
                                     return (x,y)) (seed gs)
    put $ SState (SnakeGame (gx, gy) (snake gs) (food gs) nseed) 
                 (gsMVar ggs)
    
    return $ if fpos == spos then rpos else fpos    



-- |Represents the state of the snake game
data SnakeGame =  SnakeGame { grid :: GridSize
                            , snake :: (Position, Snake)
                            , food :: (Position, Food)
                            , seed :: StdGen}



-- |This function reads from the config file to initialise 
-- the game state.
initGameState :: IO (GameState SnakeGame SnakeInputs)
initGameState = do 
  handle <- openFile "./init-data.snk" ReadMode  
  s <- hGetContents  handle
  
  iMVar <- newMVar $ SInputs SnDown  
  forkIO (getInputsThread iMVar readInputs)
  
  rgen <- newStdGen
  case parse parseSnake "snk" s of
    Left err -> return Err
    Right (grd, snk, fd) -> return $ SState (SnakeGame grd snk fd rgen) iMVar



-- |This function updates the game state according the given inputs
updateGameState :: MonadState (GameState SnakeGame SnakeInputs) m => 
                   GameInputs SnakeInputs -> m ()
updateGameState i = 
  do
    (snkpos, snk) <- updatePlayer i
    nfood <- updateFood
    gs <- get
    if isLostSnake snk
    then put GSQuit
    else case gs of      
         SState (SnakeGame grd _ _ oseed) oMVar -> put $
                                          SState (SnakeGame grd (snkpos, snk)
                                          nfood oseed) oMVar
         _ -> put gs



-- |This function draws the game state        
drawGameState :: (MonadState (GameState SnakeGame SnakeInputs) m,
                  MonadIO m) => m ()
drawGameState =
  do 
    gs <- get
    let (gx, gy) = grid $ gstate gs
    case gs of      
      SState (SnakeGame grd _ _ oseed) oMVar -> 
        liftIO $ printASCII gx ' ' $ gameStateFormat (gx, gy) $ gstate gs
      _                                      -> 
        return ()
    liftIO $ threadDelay 100000        
  where      
    gameStateFormat grd gs = zipWith mergeGameStateList 
                             (foodList  grd $ food  gs)
                             (snakeList grd $ snake gs)
                                
    mergeGameStateList :: [(Int, Char)] -> [(Int, Char)] -> [(Int, Char)]
    mergeGameStateList = foldr mergeGameStateList'

    mergeGameStateList' :: (Int, Char) -> [(Int, Char)] -> [(Int, Char)]
    mergeGameStateList' (x, ch) xs 
      | x `elem` map fst xs = xs
      | otherwise           = insertBy compareASCIICell (x, ch) xs

    --Generates the food list from the gamestate food field 
    foodList (_, gy) f = foodList' f $ replicate gy []
        

    foodList' ((x, y), _) xs = nxs
      where        
        (z:zs) = drop (y-1) xs
        nxs    = take (y-1) xs ++ (((x, 'F'):z):zs)

    --Generates the snake list from the gamestate snake field
    snakeList (gx, gy) snk = snakeList' (gx, gy) snk $ replicate gy []
                        
    snakeList' _ (_, SnTail) xs = map (sortBy compareASCIICell) xs
    snakeList' (gx, gy) ((px, py), snk) xs = snakeList' (gx, gy) 
                                             ((npx, npy), nsnk) 
                                             nxs
      where 
        nsnk | size snk <= 1 = sntail snk
             | size snk >  1 =  Head { direction = direction snk
                                     , size = size snk - 1
                                     , sntail = sntail snk
                                     }
                                    
        (y:ys) = drop (py-1) xs
        nxs    = take (py-1) xs ++ (((px, 'S'):y):ys)
                 
        (npx, npy) = case direction snk of
          SnUp    -> (px                , (py+1)    `mod` gy)
          SnDown  -> (px                , (py-1+gy) `mod` gy)
          SnLeft  -> ((px+1+gx) `mod` gx, py                )
          SnRight -> ((px-1)    `mod` gx, py                )

    compareASCIICell (idx1, _) (idx2, _) 
      | idx1 > idx2 = LT
      | idx1 == idx2 = EQ
      | otherwise   = GT
                                                



--Parses the config file and return data to initialise the game state
parseSnake :: Parser (GridSize, (Position, Snake), (Position, Food))
parseSnake = do
  gx <- many1 digit
  spaces
  gy <- many1 digit
  spaces
  sx <- many1 digit
  spaces
  sy <- many1 digit
  spaces
  sze <- many1 digit
  spaces
  fx <- many1 digit
  spaces
  fy <- many1 digit
  return ((read gx, read gy),                      
          ((read sx, read sy), Head {direction = SnDown,
                                     size = read sze,
                                     sntail = SnTail}),
          ((read fx, read fy), Food))
