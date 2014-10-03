module Data.Geo.Coordinate.Parse where

import Text.Parser.Char
import Text.Parser.Combinators
import Prelude hiding (elem)
import Control.Applicative(Alternative((<|>)), liftA3)
import Data.Foldable(elem, asum)
import Text.Parsec(parse)
import Data.Char
import Data.Bool

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
  let digit' =
        fmap digitToInt digit
      sat n =
        try (do c <- digit'
                bool3 (n * 10 + c)) <|> return n
      bool3 =
        liftA3 bool return sat p
  in do sn <- optional (char '-' <|> char '+') 
        c1 <- digit'
        d  <- bool3 c1
        return (bool id negate (elem '-' sn) d)

dg90 ::
 (CharParsing f, Monad f) =>
  f Int
dg90 =
  satisfyInteger (\n -> n <= 8)

dg180 ::
 (CharParsing f, Monad f) =>
  f Int
dg180 =
  satisfyInteger (< 180)
 