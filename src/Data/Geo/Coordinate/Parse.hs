module Data.Geo.Coordinate.Parse where

import Text.Parser.Char
import Text.Parser.Combinators
import Prelude hiding (elem)
import Control.Applicative(Alternative((<|>)), liftA3, (<$>))
import Data.Foldable(elem, asum)
import Text.Parsec(parse)
import Control.Lens(Prism', prism, (^?))
import Numeric.Lens
import Data.Char(digitToInt)
import Data.Bool
import Data.Geo.Coordinate

skipChars ::
  CharParsing f =>
  f Char
skipChars =
  asum (fmap char "?*")
   
satisfyInteger ::
  (CharParsing p, Monad p) =>
  (Int -> Bool)
  -> p Int  
satisfyInteger p =
  let digitP n g =
        do c <- digit
           liftA3 bool return (\o -> try (digitP o g) <|> return n) p (n * 10 + g (digitToInt c))
  in bool id negate . elem '-' <$> optional (char '-' <|> char '+') >>=
       digitP 0



satisfyNatural ::
  (CharParsing p, Monad p) =>
  (Int -> Bool)
  -> p Int  
satisfyNatural p =
  let sat n =
        try (digitP n) <|> return n
      digitP n =
        do c <- digit
           liftA3 bool return sat p (n * 10 + digitToInt c)
  in digitP 0 
        
parseDegreesLatitude ::
 (CharParsing f, Monad f) =>
  f DegreesLatitude
parseDegreesLatitude =
  fmap remDegreesLatitude (satisfyInteger (\n -> all ($n) [(>= -8), (<= 8)]))

parseDegreesLongitude ::
 (CharParsing f, Monad f) =>
  f DegreesLongitude
parseDegreesLongitude =
  fmap remDegreesLongitude (satisfyInteger (<= 17))

dg180 ::
 (CharParsing f, Monad f) =>
  f Int
dg180 =
  satisfyInteger (< 180)

data Sign =
  Plus
  | Minus
  | N
  | S
  deriving (Eq, Ord, Show)

parseSign ::
  CharParsing p =>
  p Sign
parseSign =
  try (Plus <$ char '+') <|>
  try (Minus <$ char '-') <|>
  try (N <$ char 'N') <|>
  try (S <$ char 'S')

data DegreesSeparator =
  Decimal
  | Degrees
  | Space
  deriving (Eq, Ord, Show)

parseDegreesSeparator ::
  CharParsing p =>
  p DegreesSeparator
parseDegreesSeparator =
  try (Decimal <$ char '.') <|>
  try (Degrees <$ char '\176') <|>
  try (Space <$ space)
  
number ::
  Integral a =>
  String
  -> a
number =
  foldl (\a b -> case [b] ^? decimal of
                   Just n -> a * 10 + n
                   Nothing -> a) 0
  

parseSignLat ::
  CharParsing p =>
  p Bool
parseSignLat =
  try (True <$ char '+') <|>
  try (False <$ char '-') <|>
  try (True <$ char 'N') <|>
  try (False <$ char 'S') <|>
  pure True

-- temp
parseDegreesLatitude2 ::
  (Monad p, CharParsing p) =>
  p DegreesLatitude
parseDegreesLatitude2 =
  do  s <- parseSignLat
      remDegreesLatitude . bool negate id s . number <$> many (noneOf "\176. \t\n\r")
