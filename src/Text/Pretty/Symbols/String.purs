module Text.Pretty.Symbols.String where

import Text.Pretty (Doc, enclose, text)

-- | >>> squotes "·"
-- '·'
squotes :: Doc String -> Doc String
squotes = enclose squote squote

-- | >>> dquotes "·"
-- "·"
dquotes :: Doc String -> Doc String
dquotes = enclose dquote dquote

-- | >>> parens "·"
-- (·)
parens :: Doc String -> Doc String
parens = enclose lparen rparen

-- | >>> angles "·"
-- <·>
angles :: Doc String -> Doc String
angles = enclose langle rangle

-- | >>> brackets "·"
-- [·]
brackets :: Doc String -> Doc String
brackets = enclose lbracket rbracket

-- | >>> braces "·"
-- {·}
braces :: Doc String -> Doc String
braces = enclose lbrace rbrace

-- | >>> squote
-- '
squote :: Doc String
squote = text "'"

-- | >>> dquote
-- "
dquote :: Doc String
dquote = text "\""

-- | >>> lparen
-- (
lparen :: Doc String
lparen = text "("

-- | >>> rparen
-- )
rparen :: Doc String
rparen = text ")"

-- | >>> langle
-- <
langle :: Doc String
langle = text "<"

-- | >>> rangle
-- >
rangle :: Doc String
rangle = text ">"

-- | >>> lbracket
-- [
lbracket :: Doc String
lbracket = text "["
-- | >>> rbracket
-- ]
rbracket :: Doc String
rbracket = text "]"

-- | >>> lbrace
-- {
lbrace :: Doc String
lbrace = text "{"
-- | >>> rbrace
-- }
rbrace :: Doc String
rbrace = text "}"

-- | >>> semi
-- ;
semi :: Doc String
semi = text ";"

-- | >>> colon
-- :
colon :: Doc String
colon = text ":"

-- | >>> comma
-- ,
comma :: Doc String
comma = text ","

-- | >>> "a" <> space <> "b"
-- a b
--
-- This is mostly used via @'<+>'@,
--
-- >>> "a" <+> "b"
-- a b
space :: Doc String
space = text " "

-- | >>> dot
-- .
dot :: Doc String
dot = text "."

-- | >>> slash
-- /
slash :: Doc String
slash = text "/"

-- | >>> backslash
-- \\

backslash :: Doc String
backslash = text "\\"

-- | >>> equals
-- =
equals :: Doc String
equals = text "="

-- | >>> pipe
-- |
pipe :: Doc String
pipe = text "|"
