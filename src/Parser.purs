module Parser (parseHutton) where
  
import Prelude

import Data.Array (some, reverse, uncons)
import Data.Hutton (Expr(..))
import Data.Maybe (Maybe(..))
import Text.Parsing.Parser (Parser)
import Text.Parsing.Parser.Expr (Assoc(..), Operator(..), buildExprParser)
import Text.Parsing.Parser.String (string)
import Text.Parsing.Parser.Token (digit)

decimal :: Parser String Expr
decimal = (Lit <<< toInt <<< reverse) <$> (some digit)
  where toInt arr = case (uncons arr) of
                        Just { head: x, tail: xs } -> charToInt x + 10 * toInt xs
                        Nothing -> 0
        charToInt x = case x of
                        '1' -> 1
                        '2' -> 2
                        '3' -> 3
                        '4' -> 4
                        '5' -> 5
                        '6' -> 6
                        '7' -> 7
                        '8' -> 8
                        '9' -> 9
                        _   -> 0

parseHutton :: Parser String Expr
parseHutton = buildExprParser [[ Infix (string "+" $> Add) AssocRight ]] decimal