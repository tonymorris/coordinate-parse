module Data.Geo.Coordinate.Parse where

import Text.Parser.Char
import Text.Parser.Combinators
import Prelude hiding (elem)
import Data.Functor
import Control.Applicative(Alternative((<|>)), Applicative(..))
import Data.Foldable(elem, asum)
import Text.Parsec(parse)
import Data.Char
import Data.Bool

data Sign =
  Neg
  | Pos
  deriving (Eq, Show)

skipChars ::
  CharParsing f =>
  f Char
skipChars =
  asum (fmap char "?*")
   
dig ::
  (CharParsing p, Monad p) =>
  (Int -> Bool)
  -> p Int  
dig p =
  let digit' = fmap digitToInt digit
      f <<*>> x = f bool return <*> dig' <*> p $ x
      dig' n =
        try (do c <- digit'
                (\b -> pure . b . ($ n)) <<*>> (n * 10 + c)) <|> return n
  in do sn <- optional (char '-' <|> char '+') 
        c1 <- digit'
        d <- (<$>) <<*>> c1
        return (bool id negate (elem '-' sn) d)

dg90 ::
 (CharParsing f, Monad f) =>
  f Int
dg90 =
  dig (< 90)

dg180 ::
 (CharParsing f, Monad f) =>
  f Int
dg180 =
  dig (< 180)
 