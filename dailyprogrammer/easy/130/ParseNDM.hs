module ParseNDM where

import Text.ParserCombinators.Parsec



readNDM :: String  -> Either String (Int, Int)
readNDM s = case parse parseNDM "ndm" s of
  Left err -> Left  ("No match for " ++ s)
  Right val -> Right val
  
parseNDM :: Parser (Int, Int)
parseNDM = do
  n <- many1 digit
  char 'D'
  m <- many1 digit
  return (read n, read m)



