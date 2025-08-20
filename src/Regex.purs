module Regex (
  Regex(..),
  parseString,
  validChar,
  parseRegex
) where

import Prelude (
  (==), (&&), (||), (<$), (<$>), ($), (>>>), (<*),
  unit, bind, discard, pure,
  class Eq, Unit
  )
import Control.Alt ((<|>))
import Control.Lazy (class Lazy, defer)
import Data.Foldable (any, length)
import Data.Array ((..), take, drop)
import Data.CodePoint.Unicode as U
import Data.Either (Either)
import Data.String.CodePoints (codePointFromChar)
import Parsing (Parser, ParseError, runParser)
import Parsing.Combinators as PC
import Parsing.String as PS

data Regex char
  = Empty
  | Epsilon
  | Char char
  | Concat (Regex char) (Regex char)
  | Union (Regex char) (Regex char)
  | Star (Regex char)

-- Could be more efficient
parseString :: forall char. Eq char => Regex char -> Array char -> Boolean
parseString Empty _ = false
parseString Epsilon [] = true
parseString (Char char1) [char2] = char1 == char2
parseString (Concat left right) string = any
  (\n -> parseString left (take n string) && parseString right (drop n string))
  (0..length string)
parseString (Union left right) string =
  parseString left string || parseString right string
parseString (Star _) [] = true
parseString (Star r) string = any
  (\n -> parseString r (take n string) && parseString (Star r) (drop n string))
  (0..length string)
parseString _ _ = false

{-
Top level regex
R := V

Simple := Empty | Epsilon | Char
Concat := C C
Union := V U V
Star := S *

Things that can be starred
S :=
  Simple |
  ( Concat ) |
  ( Union ) |
  ( Star ) |
  Star

Things that can be catted
C := Simple |
  ( Concat ) |
  ( Union ) |
  ( Star ) |
  Star |
  Concat

Things that can be unioned
V := Simple |
  ( Concat ) |
  ( Union ) |
  ( Star ) |
  Star |
  Concat |
  Union
-}

validChar :: Char -> Boolean
validChar char =
  U.isAscii (codePointFromChar char) &&
  U.isAlphaNum (codePointFromChar char)

type RegexParser = Parser String (Regex Char)

parseRegex :: String -> Either ParseError (Regex Char)
parseRegex s = runParser s $ parseUnionable <* PS.eof
  where
  parseEmpty :: RegexParser
  parseEmpty = Empty <$ PS.char '∅'

  parseEpsilon :: RegexParser
  parseEpsilon = Epsilon <$ PS.char 'ε'

  parseChar :: RegexParser
  parseChar = Char <$> PS.satisfy validChar

  parseSpaces :: Parser String Unit
  parseSpaces = unit <$ PC.many (PS.satisfy $ codePointFromChar >>> U.isSpace)

  bracket :: forall a. Parser String a -> Parser String a
  bracket p =
    PC.between (PS.char '(') (PS.char ')') p <|>
    PC.between (PS.char '[') (PS.char ']') p <|>
    PC.between (PS.char '{') (PS.char '}') p

  parseSimple :: RegexParser
  parseSimple = parseEmpty <|> parseEpsilon <|> parseChar

  parseConcat :: Lazy RegexParser => RegexParser
  parseConcat = do
    left <- defer \_ -> parseConcatable
    parseSpaces
    right <- defer \_ -> parseConcatable
    pure $ Concat left right

  parseUnion :: Lazy RegexParser => RegexParser
  parseUnion = do
    left <- defer \_ -> parseUnionable
    parseSpaces
    _ <- PS.char '|'
    parseSpaces
    right <- defer \_ -> parseUnionable
    pure $ Union left right

  parseStar :: Lazy RegexParser => RegexParser
  parseStar = do
    contents <- defer \_ -> parseStarrable
    parseSpaces
    _ <- PS.char '*'
    pure $ Star contents

  parseStarrable :: Lazy RegexParser => RegexParser
  parseStarrable =
    PC.try parseSimple <|>
    PC.try (bracket parseConcat) <|>
    PC.try (bracket parseUnion) <|>
    PC.try (bracket parseStar) <|>
    PC.try parseStar

  parseConcatable :: Lazy RegexParser => RegexParser
  parseConcatable =
    PC.try parseStarrable <|>
    PC.try parseConcat

  parseUnionable :: Lazy RegexParser => RegexParser
  parseUnionable =
    PC.try parseConcatable <|>
    PC.try parseUnion
