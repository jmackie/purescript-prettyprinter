module Test.Common
    ( runTest
    , rule
    , red
    , green
    , blue
    , yellow
    )
where

import Prelude
import Ansi.Codes as AnsiCodes
import Ansi.Output as AnsiOutput
import Data.Foldable (fold)
import Data.Unfoldable (replicate)
import Effect (Effect)
import Effect.Console as Console
import Text.Pretty as Pretty


runTest :: Int -> Pretty.Doc String -> Effect Unit
runTest width doc = do
    Console.log $ blue (rule width)
    Console.log $ Pretty.render width doc
    Console.log $ blue (rule width)
    Console.log "" -- for the newline


rule :: Int -> String
rule n = fold (replicate n "-" :: Array String)


red :: String -> String
red = AnsiOutput.withGraphics $
    AnsiOutput.foreground AnsiCodes.Red


green :: String -> String
green = AnsiOutput.withGraphics $
    AnsiOutput.foreground AnsiCodes.BrightGreen


blue :: String -> String
blue = AnsiOutput.withGraphics $
    AnsiOutput.foreground AnsiCodes.BrightBlue


yellow :: String -> String
yellow = AnsiOutput.withGraphics $
    AnsiOutput.foreground AnsiCodes.BrightYellow
