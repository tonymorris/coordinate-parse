module Data.Geo.Coordinate.Parse where

import Text.Parser.Char
import Text.Parser.Combinators
import Prelude hiding (elem)
import Data.Functor
import Control.Applicative(Alternative((<|>)))
import Data.Foldable(elem, asum)
import Text.Parsec(parse)
import Data.Char
import Data.Maybe
import Data.Bool

data Sign =
  Neg
  | Pos
  deriving (Eq, Show)

csign ::
  Sign
  -> Int
  -> Int
csign Neg n =
  negate n
csign Pos n =
  n

csign' ::
  Maybe Sign
  -> Int
  -> Int
csign' =
  csign . fromMaybe Pos

sign ::
  (CharParsing f, Alternative f) =>
  f (Maybe Sign)
sign =
  optional (Neg <$ char '-') <|> optional (Pos <$ char '+')
   
skipChars ::
  CharParsing f =>
  f Char
skipChars =
  asum (fmap char "?*")

dg180 ::
  (CharParsing f, Monad f) =>
  f Int
dg180 =
  let convert c1 c2 =
        maybe (digitToInt c1 * 10 + digitToInt c2) (\c3 -> digitToInt c1 * 100 + digitToInt c2 * 10 + digitToInt c3)
  in do sn <- sign
        c1 <- digit
        c2 <- optional digit
        case c2 of 
          Just c2' ->
            let next = bool
                         (`fmap` optional digit)
                         (return . ($ Nothing))
                         (c2' `elem` ['8', '9'])
            in next (csign' sn . convert c1 c2')
   
data Dig =
  Fail String
  | Continue
  | Return

failDig ::
  Monad m =>
  m a
  -> m a
  -> Dig
  -> m a
failDig _ _ (Fail s) =
  fail s
failDig cont _ Continue = 
  cont
failDig _ ret Return =
  ret

digit' ::
 (CharParsing f, Monad f) =>
  f Int
digit' =
  fmap digitToInt digit

dig ::
  (CharParsing p, Monad p) =>
  (Int -> Int -> Dig)
  -> p Int  
dig p =
  let dig' r n =
        try (do c <- digit'
                let n' = n * 10 + c
                    r' = r + 1
                failDig (dig' r' n') (return n') (p r' n')) <|> return n
  in do sn <- Just <$> char '-' <|> Just <$> char '+' <|> return Nothing
        let sn' = bool id negate (elem '-' sn)
        c1 <- digit'
        d <- failDig (dig' 1 c1) (return c1) (p 1 c1)
        return (sn' d)

dg90' ::
 (CharParsing f, Monad f) =>
  f Int
dg90' =
  dig (\r n ->
    bool Return Continue (r == 1 && n /= 9))

dg180' ::
 (CharParsing f, Monad f) =>
  f Int
dg180' =
  dig (\r n -> 
    bool (bool undefined undefined (r == 2)) Continue (r == 1))

dg90 ::               
  (CharParsing f, Monad f) =>
  f Int
dg90 =
  let convert c1 =
        maybe (digitToInt c1) (\c2 -> digitToInt c1 * 10 + digitToInt c2 * 1)
  in do skipMany skipChars
        sn <- sign
        skipMany (char '0' <|> skipChars)
        c1 <- digit
        skipMany skipChars
        let next = bool
                     (`fmap` optional digit)
                     (return . ($ Nothing))
                     (c1 == '9')
        next (csign' sn . convert c1)
 
q' :: 
  (CharParsing f, Alternative f, Monad f) =>
  f (Maybe Sign, Char)
q' =
  do s <- sign
     c <- char 'x'
     return (s, c)
