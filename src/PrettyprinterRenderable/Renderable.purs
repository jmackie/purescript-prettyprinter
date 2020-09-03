module PrettyprinterRenderable.Renderable
  ( class Renderable
  , space
  , newline
  , width
  ) where

import Prelude
import Data.String as String

-- | A class for things that can be rendered pretty.
class
  Monoid a <= Renderable a where
  space :: a
  newline :: a
  width :: a -> Int

instance renderableString :: Renderable String where
  space = " "
  newline = "\n"
  width = String.length
