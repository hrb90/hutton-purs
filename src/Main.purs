module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Array (some, reverse, uncons)
import Data.Bifunctor (lmap)
import Data.Maybe (Maybe(..))
import Partial.Unsafe (unsafePartial)
import Signal.Channel (CHANNEL)
import Sparkle (sparkle)
import Text.Parsing.Parser (Parser, runParser)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (digit)

data Expr = Lit Int | Add Expr Expr

eval :: Expr -> Int
eval (Lit i)     = i
eval (Add x1 x2) = eval x1 + eval x2

decimal :: Parser String Expr
decimal = some digit
            # map makeLit
  where makeLit x = x # reverse # toInt # Lit
        toInt arr = case (uncons arr) of
                        Just { head: x, tail: xs } -> charToInt x + 10 * toInt xs
                        Nothing -> 0
        charToInt x = unsafePartial $ case x of
                        '0' -> 0
                        '1' -> 1
                        '2' -> 2
                        '3' -> 3
                        '4' -> 4
                        '5' -> 5
                        '6' -> 6
                        '7' -> 7
                        '8' -> 8
                        '9' -> 9

parseHutton :: Parser String Expr
parseHutton = buildExprParser [ [ Infix (string "+" $> Add) AssocRight ] ] decimal

main :: forall eff. Eff (channel :: CHANNEL, dom :: DOM | eff) Unit
main = sparkle "Hutton's razor" parseAndEval 
  where parseAndEval = parseHutton
                          # map eval
                          # flip runParser
                          # map (lmap show)