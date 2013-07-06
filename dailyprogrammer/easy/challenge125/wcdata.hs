module WCData where

import Data.List


data Analytics = Analytics
  {
    wordNumber :: Int,
    letterNumber :: Int,
    symbolNumber :: Int,
    wordList :: [Countable String],
    letterList :: [Countable Char]
  }
  
instance Show Analytics where
  show a = show (wordNumber a) ++ " words\n" ++
           show (letterNumber a) ++ " letters\n" ++
           show (symbolNumber a) ++ " symbols\n" ++
           "Top three most comon words: " ++ show (take 3 $ reverse $ sort $ wordList a) ++ "\n" ++
           "Top three most comon letters: " ++ show (take 3 $ reverse $ sort $ letterList a) ++ "\n"
           
           
           
           
data Countable a = Countable a Int

instance Show a => Show (Countable a) where
  show (Countable a _) = show a
  
instance Eq  (Countable c) where    
  (==) (Countable _ a) (Countable _ b) = (a == b)
instance Ord (Countable c) where    
  (<=) (Countable _ a) (Countable _ b) = (a <= b)