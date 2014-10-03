module Data.Geo.Coordinate.Parse where

import Text.Parser.Char
import Text.Parser.Combinators
import Prelude hiding (elem)
import Control.Applicative(Alternative((<|>)), liftA3)
import Data.Foldable(elem, asum)
import Text.Parsec(parse)
import Data.Char
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
  let sat n =
        try (digitP n) <|> return n
      digitP n =
        do c <- digit
           liftA3 bool return sat p (n * 10 + digitToInt c)
  in do sn <- optional (char '-' <|> char '+') 
        d  <- digitP 0 
        return (bool id negate (elem '-' sn) d)

parseDegreesLatitude ::
 (CharParsing f, Monad f) =>
  f DegreesLatitude
parseDegreesLatitude =
  fmap remDegreesLatitude (satisfyInteger (<= 8))

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
 