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
updateSnake gs sn (SnakeInputs i)
  | ps == pf = if direction sn == i
               then Head {direction = direction sn,
                          size = 1 + size sn,
                          sntail = sntail sn}
               else Head {direction = i,
                          size = 1,
                          sntail = sn}
  | otherwise = if direction sn == i
                then Head {direction = direction sn,
                           size = 1 + size sn,
                           sntail = sntail $ shortenTail sn}
                else Head {direction = i,
                           size = 1,
                           sntail = shortenTail sn}
  where (ps, _) = snake gs
        (pf, _) = food gs
        shortenTail s = case sntail s of
          Head hsnk sze hsntail -> shortenTail hsntail
          SnTail -> if size s > 1
                  then Head {direction = direction s,
                             size = size s - 1,
                             sntail = SnTail}
                  else SnTail



                         
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
        snakeNfood2Str' (x:xs) (y:ys) gxidx str
          | x == gxidx && y == gxidx = snakeNfood2Str' xs ys (gxidx-1)
                                       ('S':str)
          | x == gxidx = snakeNfood2Str' xs (y:ys) (gxidx-1) ('S':str)
          | y == gxidx = snakeNfood2Str' (x:xs) ys (gxidx-1) ('F':str)
          | otherwise = snakeNfood2Str' (x:xs) (y:ys) (gxidx-1) (' ':str)
        

        foodLst ((x, y), _) z
          | y == z = [x]
          | otherwise = []

        
        
        snakeLst snk idx = snakeLst' snk idx [] 
        
        snakeLst' (_, SnTail) _ xs = sort xs
        snakeLst' ((px, py), snk) idx xs =
          snakeLst' ((npx, npy), nsnk) idx nxs
          where nsnk = if size snk > 1
                       then Head {direction = direction snk,
                                  size = 1 - size snk,
                                  sntail = sntail snk}
                       else sntail snk
                            
                nxs = if idx == py
                      then px:xs
                      else xs
                           
                (npx, npy) = case direction snk of
                  SnUp -> (px, py+1)
                  SnDown -> (px, py-1)
                  SnLeft -> (px+1, py)
                  SnRight -> (px-1, py)

                                 
                           
                         

parseSnake :: Parser SnakeGame
parseSnake = do
  gx <- many1 digit
  gy <- many1 digit
  spaces
  px <- many1 digit
  py <- many1 digit
  spaces
  sze <- many1 digit
  spaces
  sx <- many1 digit
  sy <- many1 digit
  spaces
  fx <- many1 digit
  fy <- many1 digit
  return SnakeGame {grid = (read gx, read gy),                      
                    snake = ((read sx, read sy), Head {direction = SnDown,
                                                       size = read sze,
                                                       sntail = SnTail}),
                    food = ((read fx, read fy), Food)}
                        
                             
    
  