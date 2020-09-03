-- | Frequently useful definitions for working with general prettyprinters.
module PrettyprinterRenderable.Util where

import Prelude
import PrettyprinterRenderable (Doc, fillSep, text)
import Data.String.Utils as String

-- | Split an input into word-sized 'Doc's.
--
-- >>> putDoc (tupled (words "Lorem ipsum dolor"))
-- (Lorem, ipsum, dolor)
words :: String -> Array (Doc String)
words = map text <<< String.words

-- | Insert soft linebreaks between words, so that text is broken into multiple
-- lines when it exceeds the available width.
--
-- >>> putDocW 32 (reflow "Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.")
-- Lorem ipsum dolor sit amet,
-- consectetur adipisicing elit,
-- sed do eiusmod tempor incididunt
-- ut labore et dolore magna
-- aliqua.
--
-- @
-- 'reflow' = 'fillSep' . 'words'
-- @
reflow :: String -> Doc String
reflow = fillSep <<< words
