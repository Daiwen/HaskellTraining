module SnakeGame where

import System.IO
import System.Environment
import Text.Parsec.Char
import Text.Parsec.Prim
import Data.List
import Text.ParserCombinators.Parsec

data Direction = SnUp | SnDown | SnLeft | SnRight deriving (Eq)
data SnakeInputs = SnakeInputs Direction | Unknown


getInputs :: IO SnakeInputs
getInputs = do
  x <- getChar
  return $ readInputs x
    
readInputs :: Char -> SnakeInputs
readInputs x  
  | x == 'z' = SnakeInputs SnUp
  | x == 's' = SnakeInputs SnDown
  | x == 'q' = SnakeInputs SnLeft
  | x == 'd' = SnakeInputs SnRight
  | otherwise = Unknown


type GridSize = (Int, Int)
type Position = (Int, Int)

updatePosition :: SnakeGame -> Position -> SnakeInputs -> Position
updatePosition s (a, b) (SnakeInputs SnUp) = (a, (b-1) `mod` y)
                             where (_, y) = grid s
updatePosition s (a, b) (SnakeInputs SnDown) = (a, (b+1) `mod` y)
                             where (_, y) = grid s
updatePosition s (a, b) (SnakeInputs SnLeft) = ((a-1) `mod` x, b)
                             where (x, _) = grid s
updatePosition s (a, b) (SnakeInputs SnRight) = ((a+1) `mod` x, b)
                             where (x, _) = grid s
updatePosition _ c Unknown = c


data Snake = SnTail | Head {direction :: Direction,
                            size :: Int, 
                            sntail :: Snake}


updateSnake ::SnakeGame -> Snake -> SnakeInputs -> Snake
updateSnake gs sn (SnakeInputs i) =
  let (ps, _) = snake gs
      (pf, _) = food gs
      (nsze, nsntail) = 
        case (direction sn == i, sntail sn, ps == pf) of
          (True, SnTail, True) -> (1 + size sn, SnTail)   
          (True, _, True) -> (1 + size sn, sntail sn)
          (_, _, True) -> (1, sn)
          (True, SnTail, False) -> (size sn, SnTail)   
          (True, _, False) -> (1 + size sn, sntail $ shortenTail sn)
          (_, _, False) -> (1, shortenTail sn)
  in Head {direction = i,
           size = nsze,
           sntail = nsntail}
  where shortenTail s = case sntail s of
          Head hsnk sze hsntail -> Head hsnk sze $ shortenTail hsntail
          SnTail -> if size s > 1
                    then Head {direction = direction s,
                               size = size s - 1,
                               sntail = SnTail}
                    else SnTail
updateSnake gs sn Unknown = updateSnake gs sn $ SnakeInputs $ direction sn
                         
                         
data Food = Food

updateFood :: SnakeGame -> Food -> Food
updateFood s f = f
-- TODO random position


data SnakeGame = Err |SnakeGame {grid :: GridSize,
                                 snake :: (Position, Snake),
                                 food :: (Position, Food)}



  

initGameState :: IO SnakeGame
initGameState = do 
  handle <- openFile "./init-data.snk" ReadMode  
  s <- hGetContents  handle
  case parse parseSnake "snk" s of
    Left err -> return Err
    Right val -> return val            

updateGameState :: SnakeGame -> SnakeInputs -> SnakeGame
updateGameState s i = let (ps, sn) = snake s
                          (pf, f) = food s
                      in SnakeGame {grid = grid s,
                                    snake = (updatePosition s ps i,
                                             updateSnake s sn i),
                                    food = (pf, updateFood s f)}

    
    
drawGameState :: SnakeGame -> IO ()    
drawGameState gs = putStr $ gameStateStr gs



gameStateStr gs = " " ++ replicate gx '_' ++ "\n" ++
                  gameStateLine gs gy ++
                  " " ++ replicate gx '_' ++ "\n"
  where (gx, gy) = grid gs
        
        --TODO terminal recursion or monad plus?
        gameStateLine gs 0 = []
        gameStateLine gs gyidx = "|" ++ gameStateLine' gs gyidx ++
                              "|\n" ++ gameStateLine gs (gyidx - 1)
                              
        gameStateLine' gs gyidx = snakeNfood2Str (snakeLst (snake gs) gyidx)
                                  (foodLst (food gs) gyidx)
                               
        snakeNfood2Str xs ys = snakeNfood2Str' xs ys gx ""
        
        snakeNfood2Str' _ _ 0 str = reverse str
        snakeNfood2Str' [] [] gxidx str = 
          snakeNfood2Str' [] [] 0 (replicate gxidx ' ' ++ str)
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
        
        snakeLst' (_, SnTail) _ xs = sort xs
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
                  SnDown -> (px, (py-1-gy) `mod` gy)
                  SnLeft -> (px+1, py)
                  SnRight -> (px-1, py)

                                 
                           
                         

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
                    food = ((read fx, read fy), Food)}
                        
                             
    
  