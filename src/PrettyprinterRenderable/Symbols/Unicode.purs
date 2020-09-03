-- | A collection of predefined Unicode values outside of ASCII range. For
-- ASCII, see "Prettyprinter.Symbols.Ascii".
module PrettyprinterRenderable.Symbols.Unicode where

import PrettyprinterRenderable (Doc, enclose, text)

-- | Double „99-66“ quotes, as used in German typography.
--
-- >>> putDoc (d9966quotes "·")
-- „·“
d9966quotes :: Doc String -> Doc String
d9966quotes = enclose b99dquote t66dquote

-- | Double “66-99” quotes, as used in English typography.
--
-- >>> putDoc (d6699quotes "·")
-- “·”
d6699quotes :: Doc String -> Doc String
d6699quotes = enclose t66dquote t99dquote

-- | Single ‚9-6‘ quotes, as used in German typography.
--
-- >>> putDoc (s96quotes "·")
-- ‚·‘
s96quotes :: Doc String -> Doc String
s96quotes = enclose b9quote t6quote

-- | Single ‘6-9’ quotes, as used in English typography.
--
-- >>> putDoc (s69quotes "·")
-- ‘·’
s69quotes :: Doc String -> Doc String
s69quotes = enclose t6quote t9quote

-- | Double «guillemets», pointing outwards (without adding any spacing).
--
-- >>> putDoc (dGuillemetsOut "·")
-- «·»
dGuillemetsOut :: Doc String -> Doc String
dGuillemetsOut = enclose ldGuillemet rdGuillemet

-- | Double »guillemets«, pointing inwards (without adding any spacing).
--
-- >>> putDoc (dGuillemetsIn "·")
-- »·«
dGuillemetsIn :: Doc String -> Doc String
dGuillemetsIn = enclose rdGuillemet ldGuillemet

-- | Single ‹guillemets›, pointing outwards (without adding any spacing).
--
-- >>> putDoc (sGuillemetsOut "·")
-- ‹·›
sGuillemetsOut :: Doc String -> Doc String
sGuillemetsOut = enclose lsGuillemet rsGuillemet

-- | Single ›guillemets‹, pointing inwards (without adding any spacing).
--
-- >>> putDoc (sGuillemetsIn "·")
-- ›·‹
sGuillemetsIn :: Doc String -> Doc String
sGuillemetsIn = enclose rsGuillemet lsGuillemet

-- | Bottom „99“ style double quotes.
--
-- >>> putDoc b99dquote
-- „
b99dquote :: Doc String
b99dquote = text "„"

-- | Top “66” style double quotes.
--
-- >>> putDoc t66dquote
-- “
t66dquote :: Doc String
t66dquote = text "“"

-- | Top “99” style double quotes.
--
-- >>> putDoc t99dquote
-- ”
t99dquote :: Doc String
t99dquote = text "”"

-- | Bottom ‚9‘ style single quote.
--
-- >>> putDoc b9quote
-- ‚
b9quote :: Doc String
b9quote = text "‚"

-- | Top ‘66’ style single quote.
--
-- >>> putDoc t6quote
-- ‘
t6quote :: Doc String
t6quote = text "‘"

-- | Top ‘9’ style single quote.
--
-- >>> putDoc t9quote
-- ’
t9quote :: Doc String
t9quote = text "’"

-- | Right-pointing double guillemets
--
-- >>> putDoc rdGuillemet
-- »
rdGuillemet :: Doc String
rdGuillemet = text "»"

-- | Left-pointing double guillemets
--
-- >>> putDoc ldGuillemet
-- «
ldGuillemet :: Doc String
ldGuillemet = text "«"

-- | Right-pointing single guillemets
--
-- >>> putDoc rsGuillemet
-- ›
rsGuillemet :: Doc String
rsGuillemet = text "›"

-- | Left-pointing single guillemets
--
-- >>> putDoc lsGuillemet
-- ‹
lsGuillemet :: Doc String
lsGuillemet = text "‹"

-- | >>> putDoc bullet
-- •
bullet :: Doc String
bullet = text "•"

-- | >>> putDoc endash
-- –
endash :: Doc String
endash = text "–"

-- | >>> putDoc euro
-- €
euro :: Doc String
euro = text "€"

-- | >>> putDoc cent
-- ¢
cent :: Doc String
cent = text "¢"

-- | >>> putDoc yen
-- ¥
yen :: Doc String
yen = text "¥"

-- | >>> putDoc pound
-- £
pound :: Doc String
pound = text "£"
