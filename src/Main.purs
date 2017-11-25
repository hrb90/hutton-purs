module Main where

import Prelude

import Control.Monad.Eff (Eff)
import DOM (DOM)
import Data.Bifunctor (lmap)
import Data.Hutton (eval)
import Parser (parseHutton)
import Signal.Channel (CHANNEL)
import Sparkle (sparkle)
import Text.Parsing.Parser (runParser)

main :: forall eff. Eff (channel :: CHANNEL, dom :: DOM | eff) Unit
main = sparkle "Hutton's razor" parseAndEval where
  parseAndEval = (lmap $ show) <$> (flip runParser) (eval <$> parseHutton)