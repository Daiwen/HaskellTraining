module SnakeGame where

import System.IO
import System.Environment

data Direction = Up | Down | Left | Right
data SnakeInputs = Direction | Unknown

instance GameInputs SnakeInputs where
  getInputs = do
    x <- getChar
    return readInputs x
    
readInputs :: Char -> SnakeInputs
readInputs x  
| x == 'z' = Up
| x == 's' = Down
| x == 'q' = Left
| x == 'd' = Right
| _ = Unknown


data GridSize = (Int, Int)
data Position = (Int, Int)

updatePosition :: GameState a => a -> Position -> SnakeInputs -> Position
updatePosition s (a, b) Up = (a, (b-1) `mod` y)
                             where (_, y) = grid s
updatePosition s (a, b) Down = (a, (b+1) `mod` y)
                             where (_, y) = grid s
updatePosition s (a, b) Left = ((a-1) `mod` x, b)
                             where (x, _) = grid s
updatePosition s (a, b) Right = ((a+1) `mod` x, b)
                             where (x, _) = grid s
updatePosition _ c Unknown = c


data Snake = Tail | Head {direction :: Direction,
                          size :: Int, 
                          tail :: Snake}
             
updateSnake :: GameState a => a -> Snake -> SnakeInputs -> Snake
updateSnake gs sn i 
  | ps == pf = if (direction sn) == i
               then Head {direction = direction sn,
                          size = 1 + $ size sn,
                          tail = tail sn}
               else Head {direction = i,
                          size = 1,
                          tail = sn}
  | otherwise = if (direction sn) == i
                then Head {direction = direction sn,
                           size = 1 + $ size sn,
                           tail = tail $ shortenTail sn}
                else Head {direction = i,
                           size = 1,
                           tail = shortenTail sn}
  where (ps, _) = snake gs
        (pf, _) = food gs
        shortenTail s = case tail s of
          Head -> shortenTail $ tail s
          Tail -> if (size s) > 1
                  then Head {direction = direction s,
                             size = (size s) - 1,
                             tail = Tail}
                  else Tail



                         
data Food = Food

updateFood :: GameState a => a -> Food -> Food
updateFood f = f
-- TODO random position


data SnakeGame = Err |SnakeGame {grid :: GridSize,
                                 snake :: (Position, Snake),
                                 food :: (Position, Food)}



  
instance GameState SnakeGame where
  initGameState = do 
    handle <- openFile "./init-data.snk" ReadMode  
    s <- hGetContents  handle
    case parse parseSnake "snk" s of
      Left err -> return Err
      Right val -> return val            
  
  updateGameState s i = SnakeGame {grid = grid s,
                                   snake = (updatePosition s ps i,
                                            updateSnake s sn i),
                                   food = (pf, updateFood s f)}
                        where (ps, sn) = snake s
                              (pf, f) = food f

    
    
    
  drawGameState
  --TODO implement
  

parseSnake :: Parser SnakeGame
parseSnake = do
  gx <- many1 digit
  gy <- many1 digit
  skipSpaces
  px <- many1 digit
  py <- many1 digit
  skipSpaces
  sze <- many1 digit
  skipSpaces
  sx1 <- many1 digit
  sy1 <- many1 digit
  sx2 <- many1 digit
  sy2 <- many1 digit
  skipSpaces
  fx <- many1 digit
  fy <- many1 digit
  return $ SnakeGame {grid = (gx, gy),                      
                      snake = Snake {pos = (px, py),
                                     size = sze,
                                     turn = (sx1, sy1):(sx2,sy2)},
                      food = (fx, fy)}
                        
                             
    
  