module SnakeGame where


data SnakeGame = SnakeGame 
                 {
                 }

instance GameState SnakeGame where
  initGameState
  nextGameState
  updateGameState
  drawGameState