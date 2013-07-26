module WCData where

import Data.List


data Analytics = Analytics
  {wordNumber :: Int,
   letterNumber :: Int,
   symbolNumber :: Int,
   wordList :: [Countable String],
   letterList :: [Countable Char]}
  
instance Show Analytics where
  show a = show (wordNumber a) ++ " words\n" ++
           show (letterNumber a) ++ " letters\n" ++
           show (symbolNumber a) ++ " symbols\n" ++
           "Top three most comon words: " ++
           show (take 3 $ reverse $ sort $ wordList a) ++ "\n" ++
           "Top three most comon letters: " ++
           show (take 3 $ reverse $ sort $ letterList a) ++ "\n"
           
           
           
addWord :: Analytics -> String -> Analytics
addWord a b = Analytics {wordNumber = 1 + wordNumber a,
                         letterNumber = letterNumber a,
                         symbolNumber = symbolNumber a,
                         wordList = addCountable (wordList a) b,
                         letterList = letterList a}
  
  
  
addLetter :: Analytics -> Char -> Analytics
addLetter a b = Analytics {wordNumber =  wordNumber a,
                           letterNumber = 1 + letterNumber a,
                           symbolNumber = symbolNumber a,
                           wordList = wordList a,
                           letterList = addCountable (letterList a) b}
  
addSymbol :: Analytics -> Char -> Analytics           
addSymbol a _ = Analytics {wordNumber =  wordNumber a,
                           letterNumber = letterNumber a,
                           symbolNumber = 1 + symbolNumber a,
                           wordList = wordList a,
                           letterList = letterList a}



data Countable a = Countable a Int

instance Show a => Show (Countable a) where
  show (Countable a _) = show a
  
instance Eq  (Countable c) where    
  (==) (Countable _ a) (Countable _ b) = (a == b)
instance Ord (Countable c) where    
  (<=) (Countable _ a) (Countable _ b) = (a <= b)
  
isSameEl :: Eq a => a -> Countable a -> Bool
isSameEl a (Countable b _) = a == b

addCountable ::Eq a => [Countable a] -> a -> [Countable a]
addCountable a b = 
  case x of
    [] -> ((Countable b 1):a)
    ((Countable _ c):[])-> ((Countable b (c + 1)):xs)
    ((Countable _ c):ys)-> ((Countable b (c + 1 + sumCountable ys)):xs)
    where (x,xs) = partition (isSameEl b) a
           
sumCountable :: [Countable a] -> Int
sumCountable [] = 0
sumCountable ((Countable _ a):xs) = a + sumCountable xs

