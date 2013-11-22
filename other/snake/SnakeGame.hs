module SnakeGame where

import System.IO
import System.Random
import System.Environment
import Text.Parsec.Char
import Text.Parsec.Prim
import Data.List
import Text.ParserCombinators.Parsec
import Control.Concurrent
import Control.Monad.State

data Direction = SnUp | SnDown | SnLeft | SnRight deriving (Eq)
data SnakeInputs = SnakeInputs Direction | Quit |Unknown deriving (Eq)


getInputs :: MVar SnakeInputs -> IO ()
getInputs iMVar = do
  x <- getChar
  swapMVar iMVar $ readInputs x
  getInputs iMVar
    
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

updateSnakePosition :: SnakeGame -> Position -> SnakeInputs -> Position
updateSnakePosition s (a, b) (SnakeInputs SnUp) = (a, (b-1) `mod` y)
  where (_, y) = grid s
updateSnakePosition s (a, b) (SnakeInputs SnDown) = (a, (b+1) `mod` y)
  where (_, y) = grid s
updateSnakePosition s (a, b) (SnakeInputs SnLeft) = ((a-1) `mod` x, b)
  where (x, _) = grid s
updateSnakePosition s (a, b) (SnakeInputs SnRight) = ((a+1) `mod` x, b)
  where (x, _) = grid s
updateSnakePosition s c Unknown = 
  updateSnakePosition s c $ SnakeInputs $ direction snk 
  where (_, snk) = snake s


data Snake = SnTail | Head {direction :: Direction,
                            size :: Int, 
                            sntail :: Snake}


updatePlayer :: SnakeGame -> (Position, Snake) -> SnakeInputs ->
                (Position, Snake)
updatePlayer gs (pos, snk) si = (updateSnakePosition gs pos si,
                                 updateSnake gs snk si)

updateSnake :: SnakeGame -> Snake -> SnakeInputs -> Snake
updateSnake GSQuit a b = updateSnake GSQuit a b
updateSnake gs sn (SnakeInputs i) =
  let (ps, _) = snake gs
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
  in Head {direction = i,
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
updateSnake gs sn Unknown = updateSnake gs sn $ SnakeInputs $ direction sn
                         
                         
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

updateFood :: SnakeGame -> (Position, Food) -> Position -> (Position, Food)
updateFood gs (pos, f) rpos = (updateFoodPosition gs pos rpos, f)


updateFoodPosition :: SnakeGame -> Position -> Position -> Position
updateFoodPosition gs pos rpos = if pos == p 
                                 then rpos
                                 else pos
  where (p, _) = snake gs


data SnakeGame = Err | GSQuit | SnakeGame {grid :: GridSize,
                                           snake :: (Position, Snake),
                                           food :: (Position, Food),
                                           seed :: StdGen}

instance Eq SnakeGame where
  Err == Err = True
  GSQuit == GSQuit = True
  _ == _ = False

  

initGameState :: IO SnakeGame
initGameState = do 
  handle <- openFile "./init-data.snk" ReadMode  
  s <- hGetContents  handle
  case parse parseSnake "snk" s of
    Left err -> return Err
    Right val -> return val            

updateGameState :: SnakeGame -> SnakeInputs -> SnakeGame
updateGameState s i
  | isLostSnake snk = GSQuit
  | otherwise = SnakeGame {grid = grid s,
                           snake = updatePlayer s (snake s) i,
                           food = updateFood s (food s) rpos,
                           seed = nseed}
                
  where (_, snk) = snake s
        (gx, gy) = grid s
        (rpos, nseed) = runState (do 
                                     x <- state $ randomR (0, gx-1)
                                     y <- state $ randomR (0, gy-1)
                                     return (x,y)) 
                         $ seed s
    
drawGameState :: SnakeGame -> IO ()    
drawGameState GSQuit = return ()
drawGameState gs = putStr $ gameStateStr gs



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

                                 
                           
                         

parseSnake :: Parser SnakeGame
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
  return SnakeGame {grid = (read gx, read gy),                      
                    snake = ((read sx, read sy), Head {direction = SnDown,
                                                       size = read sze,
                                                       sntail = SnTail}),
                    food = ((read fx, read fy), Food),
                    seed = mkStdGen 5}
                        
                             
    
  