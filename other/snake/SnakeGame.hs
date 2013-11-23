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

data Direction = SnUp | SnDown | SnLeft | SnRight deriving (Eq)
data SnakeInputs = SnakeInputs Direction | Quit | Unknown deriving (Eq)


getInputsThread :: MVar SnakeInputs -> IO ()
getInputsThread iMVar = do
  x <- getChar
  swapMVar iMVar $ readInputs x
  getInputsThread iMVar
    
readInputs :: Char -> SnakeInputs
readInputs x  
  | x == 'z' = SnakeInputs SnUp
  | x == 's' = SnakeInputs SnDown
  | x == 'q' = SnakeInputs SnLeft
  | x == 'd' = SnakeInputs SnRight
  | x == '\ESC' = Quit
  | otherwise = Unknown


type GridSize = (Int, Int)
type Position = (Int, Int)

data Snake = SnTail | Head {direction :: Direction,
                            size :: Int, 
                            sntail :: Snake}


updatePlayer :: MonadState SnakeGame m => SnakeInputs -> 
                m (Position, Snake)
updatePlayer si = 
  do
    npos <- updateSnakePosition si
    nsnk <- updateSnake si
    return (npos, nsnk)


updateSnakePosition :: MonadState SnakeGame m => SnakeInputs -> m Position
updateSnakePosition si =
  do 
    gs <- get     
    let (spos, snk) = snake gs
        sndir = if Unknown == si
                then SnakeInputs $ direction snk
                else si                              
    return $ updateSnakePosition' (grid gs) spos sndir
  where 
    updateSnakePosition' (gx, gy) (a, b) (SnakeInputs SnUp) =
      (a, (b-1) `mod` gy)
    updateSnakePosition' (gx, gy) (a, b) (SnakeInputs SnDown) = 
      (a, (b+1) `mod` gy)
    updateSnakePosition' (gx, gy) (a, b) (SnakeInputs SnLeft) =
      ((a-1) `mod` gx, b)
    updateSnakePosition' (gx, gy) (a, b) (SnakeInputs SnRight) =
      ((a+1) `mod` gx, b)


updateSnake :: MonadState SnakeGame m => SnakeInputs -> m Snake
updateSnake Unknown = 
  do
    gs <- get
    let (_, snk) = snake gs
    updateSnake $ SnakeInputs $ direction snk
updateSnake (SnakeInputs i) =
  do
    gs <- get
    let (ps, sn) = snake gs
        (pf, _) = food gs
        (nsze, nsntail) = 
          case (ps == pf, direction sn == i, sntail sn) of
            (True, True, SnTail) -> (1 + size sn, SnTail)   
            (True, True, _) -> (1 + size sn, sntail sn)
            (True, False, _) -> (1, sn)
            (False, True, SnTail) -> (size sn, SnTail)   
            (False, False, SnTail) -> (1, Head (direction sn) (size sn -1) SnTail)
            (False, True, _) -> (1 + size sn, shortenTail $ sntail sn)
            (False, False, _) -> (1, shortenTail sn)
    return Head {direction = i,
                 size = nsze,
                 sntail = nsntail}
  where shortenTail s = case sntail s of
          Head hsnk sze hsntail -> Head (direction s)
                                   (size s) (shortenTail $ sntail s)
          SnTail -> if size s > 1
                    then Head {direction = direction s,
                               size = size s - 1,
                               sntail = SnTail}
                    else SnTail
                         
                         
isLostSnake :: Snake -> Bool
isLostSnake snk = isLostSnake' nsnk vcpt hcpt
  where nsnk = if size snk > 1
               then Head (direction snk) (size snk - 1) (sntail snk)
               else sntail snk
        (vcpt, hcpt) = case direction snk of
          SnRight -> (0, 1)
          SnLeft -> (0, -1)
          SnUp -> (1, 0)
          SnDown -> (-1, 0)
        isLostSnake' _ 0 0 = True
        isLostSnake' SnTail _ _ = False
        isLostSnake' (Head dir sze stail) vcpt hcpt =
          isLostSnake' nsnk (vcpt+v) (hcpt+h) ||
          isLostSnake' nsnk (0+v) (0+h)
          where nsnk = if sze > 1
                       then Head dir (sze-1) stail
                       else stail
                (v,h) = case dir of
                  SnRight -> (0, 1)
                  SnLeft -> (0, -1)
                  SnUp -> (1, 0)
                  SnDown -> (-1, 0)


data Food = Food

updateFood :: MonadState SnakeGame m => m (Position, Food)
updateFood = 
  do 
    gs <- get 
    let (_, f) = food gs
    fpos <- updateFoodPosition
    return (fpos, f)


updateFoodPosition :: MonadState SnakeGame m => m Position
updateFoodPosition = 
  do 
    gs <- get
    let (fpos, _) = food gs
        (spos, _) = snake gs        
        (gx, gy) = grid gs
        (rpos, nseed) = runState (do x <- state $ randomR (0, gx-1)
                                     y <- state $ randomR (0, gy-1)
                                     return (x,y)) (seed gs)
    put $ SnakeGame (gx, gy) (snake gs) (food gs) nseed (gsMVar gs)
    
    return $ if fpos == spos then rpos else fpos    




data SnakeGame = Err | GSQuit | SnakeGame {grid :: GridSize,
                                           snake :: (Position, Snake),
                                           food :: (Position, Food),
                                           seed :: StdGen,
                                           gsMVar :: MVar SnakeInputs}

instance Eq SnakeGame where
  Err == Err = True
  GSQuit == GSQuit = True
  _ == _ = False

  
initGameState :: IO SnakeGame
initGameState = do 
  handle <- openFile "./init-data.snk" ReadMode  
  s <- hGetContents  handle
  
  iMVar <- newMVar $ SnakeInputs SnDown  
  forkIO (getInputsThread iMVar)
  
  rgen <- newStdGen
  case parse parseSnake "snk" s of
    Left err -> return Err
    Right (grd, snk, fd) -> return $ SnakeGame grd
                 snk fd rgen iMVar


getInputs :: (MonadState SnakeGame m, MonadIO m) => m SnakeInputs
getInputs = 
  do
    gs <- get
    case gs of      
         SnakeGame grd _ _ oseed oMVar -> 
           do
             let iMVar = gsMVar gs
             liftIO $ swapMVar iMVar Unknown
         _ -> return Quit


updateGameState :: MonadState SnakeGame m => SnakeInputs -> m ()
updateGameState i = 
  do
    (snkpos, snk) <- updatePlayer i
    nfood <- updateFood
    gs <- get
    if isLostSnake snk
    then put GSQuit
    else case gs of      
         SnakeGame grd _ _ oseed oMVar -> put $
                                          SnakeGame grd (snkpos, snk)
                                          nfood oseed oMVar
         _ -> put gs

    
drawGameState :: (MonadState SnakeGame m, MonadIO m) => m ()
drawGameState =
  do 
    gs <- get
    case gs of      
      SnakeGame grd _ _ oseed oMVar -> liftIO $ putStr $ gameStateStr gs
      _ -> return ()
    liftIO $ threadDelay 100000        

gameStateStr gs = " " ++ replicate gx '_' ++ "\n" ++
                  gameStateLine gs 0 ++
                  " " ++ replicate gx '_' ++ "\n"
  where (gx, gy) = grid gs
        
        --TODO terminal recursion or monad plus?
        gameStateLine gs gyidx 
          | gy == gyidx = []
          | otherwise = "|" ++ gameStateLine' gs gyidx ++
                        "|\n" ++ gameStateLine gs (gyidx + 1)
                              
        gameStateLine' gs gyidx = snakeNfood2Str (snakeLst (snake gs) gyidx)
                                  (foodLst (food gs) gyidx)
                               
        snakeNfood2Str xs ys = snakeNfood2Str' xs ys (gx-1) ""
        
        snakeNfood2Str' _ _ (-1) str = str
        snakeNfood2Str' [] [] gxidx str = 
          snakeNfood2Str' [] [] (-1) (replicate (gxidx+1) ' ' ++ str)
        snakeNfood2Str' (x:xs) (y:ys) gxidx str
          | x == gxidx && y == gxidx = snakeNfood2Str' xs ys (gxidx-1)
                                       ('S':str)
          | x == gxidx = snakeNfood2Str' xs (y:ys) (gxidx-1) ('S':str)
          | y == gxidx = snakeNfood2Str' (x:xs) ys (gxidx-1) ('F':str)
          | otherwise = snakeNfood2Str' (x:xs) (y:ys) (gxidx-1) (' ':str)
        snakeNfood2Str' xs (y:ys) gxidx str
          | y == gxidx = snakeNfood2Str' xs ys (gxidx-1) ('F':str)
          | otherwise = snakeNfood2Str' xs (y:ys) (gxidx-1) (' ':str)
        snakeNfood2Str' (x:xs) ys gxidx str
          | x == gxidx = snakeNfood2Str' xs ys (gxidx-1) ('S':str)
          | otherwise = snakeNfood2Str' (x:xs) ys (gxidx-1) (' ':str)
        

        foodLst ((x, y), _) z
          | y == z = [x]
          | otherwise = []
        
        snakeLst snk idx = snakeLst' snk idx [] 
        
        snakeLst' (_, SnTail) _ xs = reverse $ sort xs
        snakeLst' ((px, py), snk) idx xs =
          snakeLst' ((npx, npy), nsnk) idx nxs
          where nsnk = if size snk > 1
                       then Head {direction = direction snk,
                                  size = size snk - 1,
                                  sntail = sntail snk}
                       else sntail snk
                            
                nxs = if idx == py
                      then px:xs
                      else xs
                           
                (npx, npy) = case direction snk of
                  SnUp -> (px, (py+1) `mod` gy)
                  SnDown -> (px, (py-1+gy) `mod` gy)
                  SnLeft -> ((px+1+gx) `mod` gx, py)
                  SnRight -> ((px-1) `mod` gx, py)
                                                                            

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
