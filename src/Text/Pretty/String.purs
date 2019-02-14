module Text.Pretty.String
    (
    -- Bracketing combinators
      squotes, dquotes, parens, angles, brackets, braces

    -- Character documents
    , lparen, rparen, langle, rangle, lbrace, rbrace, lbracket, rbracket
    , squote, dquote, semi, colon, comma
    )
where

import Text.Pretty (Doc, enclose, text)


-- BRACKETING


squotes :: Doc String -> Doc String
squotes = enclose squote squote


dquotes :: Doc String -> Doc String
dquotes = enclose dquote dquote


parens :: Doc String -> Doc String
parens = enclose lparen rparen


angles :: Doc String -> Doc String
angles = enclose langle rangle


brackets :: Doc String -> Doc String
brackets = enclose lbracket rbracket


braces :: Doc String -> Doc String
braces = enclose lbrace rbrace


-- CHARACTERS


lparen :: Doc String
lparen = text "("


rparen :: Doc String
rparen = text ")"


langle :: Doc String
langle = text "<"


rangle :: Doc String
rangle = text ">"


lbrace :: Doc String
lbrace = text "{"


rbrace :: Doc String
rbrace = text "}"


lbracket :: Doc String
lbracket = text "["


rbracket :: Doc String
rbracket = text "]"


squote :: Doc String
squote = text "'"


dquote :: Doc String
dquote = text "\""


semi :: Doc String
semi = text ";"


colon :: Doc String
colon = text ":"


comma :: Doc String
comma = text ","
