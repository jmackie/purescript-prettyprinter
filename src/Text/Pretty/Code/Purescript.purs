module Text.Pretty.Code.Purescript where

import Prelude (($), (<<<), (<>), (==))
import Text.Pretty (Doc, align, cat, flatAlt, group, hcat, punctuate, space, text)
import Data.Renderable (class Renderable)
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
      flatAlt
      (cat $ Array.mapWithIndex
        (\i doc ->
          if i == 0
            then leftDelimiter <> space <> align doc
            else separator <> align doc
        )
        docs
        <>
        [rightDelimiter]
      )
      (leftDelimiter <> hcat (punctuate separator docs) <> rightDelimiter)

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
