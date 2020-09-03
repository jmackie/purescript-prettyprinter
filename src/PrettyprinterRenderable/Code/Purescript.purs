module PrettyprinterRenderable.Code.Purescript where

import Prelude
import PrettyprinterRenderable (Doc, align, flatAlt, group, space, text, vcat)
import PrettyprinterRenderable.Renderable (class Renderable)
import Data.Array as Array

encloseSep
    :: forall a
     . Renderable a
    => Doc a   -- ^ left delimiter
    -> Doc a   -- ^ right delimiter
    -> Doc a   -- ^ separator
    -> Array (Doc a) -- ^ input documents
    -> Doc a
encloseSep leftDelimiter rightDelimiter separator = case _ of
    []   -> leftDelimiter <> rightDelimiter
    docs ->
      let
        leftDelimiter' = flatAlt (leftDelimiter <> space) leftDelimiter
      in vcat $ (Array.zipWith (<>) ([leftDelimiter'] <> Array.replicate (Array.length docs - 1) separator) (map align docs)) <> [rightDelimiter]

-- | Purescript-inspired variant of 'encloseSep' with braces and comma as
-- separator.
--
-- >>> let doc = list (map pretty [1,20,300,4000])
--
-- >>> putDocW 80 doc
-- [1, 20, 300, 4000]
--
-- >>> putDocW 10 doc
-- [ 1
-- , 20
-- , 300
-- , 4000
-- ]
list :: Array (Doc String) -> Doc String
list = group <<< encloseSep (text "[") (text "]") (text ", ")

-- | Purescript-inspired variant of 'encloseSep' with parentheses and comma as
-- separator.
--
-- >>> let doc = tupled (map pretty [1,20,300,4000])
--
-- >>> putDocW 80 doc
-- (1, 20, 300, 4000)
--
-- >>> putDocW 10 doc
-- ( 1
-- , 20
-- , 300
-- , 4000
-- )
tupled :: Array (Doc String) -> Doc String
tupled = group <<< encloseSep (text "(") (text ")") (text ", ")
