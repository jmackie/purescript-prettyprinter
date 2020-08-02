module Text.Pretty.Code.Haskell where

import Prelude
import Text.Pretty
import Data.Renderable (class Renderable)
import Data.Array as Array

-- | @('encloseSep' l r sep xs)@ concatenates the documents @xs@ separated by
-- @sep@, and encloses the resulting document by @l@ and @r@.
--
-- The documents are laid out horizontally if that fits the page,
--
-- >>> let doc = "list" <+> align (encloseSep lbracket rbracket comma (map pretty [1,20,300,4000]))
-- >>> putDocW 80 doc
-- list [1,20,300,4000]
--
-- If there is not enough space, then the input is split into lines entry-wise
-- therwise they are laid out vertically, with separators put in the front:
--
-- >>> putDocW 10 doc
-- list [1
--      ,20
--      ,300
--      ,4000]
--
-- Note that @doc@ contains an explicit call to 'align' so that the list items
-- are aligned vertically.
--
-- For putting separators at the end of entries instead, have a look at
-- 'punctuate'.
encloseSep
    :: forall a
     . Renderable a
    => Doc a   -- ^ left delimiter
    -> Doc a   -- ^ right delimiter
    -> Doc a   -- ^ separator
    -> Array (Doc a) -- ^ input documents
    -> Doc a
encloseSep l r s ds = case ds of
    []  -> l <> r
    [d] -> l <> d <> r
    _   -> cat (Array.zipWith (<>) ([l] <> Array.replicate (Array.length ds - 1) s) (map align ds)) <> r

-- | Haskell-inspired variant of 'encloseSep' with braces and comma as
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
-- , 4000 ]
list :: Array (Doc String) -> Doc String
list = group <<< encloseSep (flatAlt (text "[ ") (text "["))
                            (flatAlt (text " ]") (text "]"))
                            (text ", ")

-- | Haskell-inspired variant of 'encloseSep' with parentheses and comma as
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
-- , 4000 )
tupled :: Array (Doc String) -> Doc String
tupled = group <<< encloseSep (flatAlt (text "( ") (text "("))
                              (flatAlt (text " )") (text ")"))
                              (text ", ")
