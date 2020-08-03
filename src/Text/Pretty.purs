module Text.Pretty (
  -- | Functions from original haskell library
  -- | https://hackage.haskell.org/package/prettyprinter-1.7.0/docs/src/Prettyprinter.Internal.html

  -- * Documents
  Doc(..),

  -- * Basic functionality
  -- | Pretty(..),
  -- | viaShow, unsafeViaShow, unsafeTextWithoutNewlines,
  emptyDoc, nest, line, line', softline, softline', hardline,

  -- ** Primitives for alternative layouts
  group, flatAlt,

  -- * Alignment functions
  align, hang, indent,

  -- * Binary functions
  surroundWithSpace, (<+>),

  -- * List functions
  concatWith,

  -- ** 'sep' family
  hsep, vsep, fillSep, sep,
  -- ** 'cat' family
  hcat, vcat, fillCat, cat,
  -- ** Others
  punctuate,

  -- * Reactive/conditional layouts
  column, nesting, width, -- pageWidth,

  -- * Filler functions
  fill, fillBreak,

  -- * General convenience
  plural, enclose, surround,

  -- ** Annotations
  -- | annotate,
  -- | unAnnotate,
  -- | reAnnotate,
  -- | alterAnnotations,
  -- | unAnnotateS,
  -- | reAnnotateS,
  -- | alterAnnotationsS,

  -- * Optimization
  -- | fuse, FusionDepth(..), -- TODO

  -- * Layout
  SimpleDocStream(..),
  -- | PageWidth(..), defaultPageWidth,
  -- | LayoutOptions(..), defaultLayoutOptions,
  -- | layoutPretty, layoutCompact, layoutSmart,
  -- | removeTrailingWhitespace,

  -- * Rendering
  render,

  -- * Internal helpers
  spaces,

  -- | Functions from this library, that are not present in haskell library
  text, flatAltFn, space, concatWithNonEmpty, surroundOmittingEmpty, vcatOmittingEmpty, vcatOmittingEmptyNonEmpty
) where

-- NOTE: Think of and build your layout in its narrowest form, then `group` the
-- parts that _could_ be flattened if there's space available.

import Prelude

import Control.Alt ((<|>))
import Data.Array.NonEmpty (NonEmptyArray)
import Data.Array.NonEmpty as NonEmptyArray
import Data.Container.Class (class Container)
import Data.Container.Class as Container
import Data.Foldable (class Foldable, fold, foldl, intercalate)
import Data.Lazy (Lazy)
import Data.Lazy as Lazy
import Data.List (List(..), (:))
import Data.Maybe (Maybe(Just, Nothing))
import Data.Renderable (class Renderable)
import Data.Renderable as Renderable
import Data.Tuple (Tuple(Tuple))
import Data.Unfoldable (replicate)
import Partial (crashWith)
import Partial.Unsafe (unsafePartial)

-- | `Doc` represents a tree of possible document layouts.
data Doc a
    -- The empty document.
    = Empty

    | Fail

    | Cat (Doc a) (Doc a)

    -- Document nested to the given indentation level
    | Nest Int (Doc a)

    -- Fragment of `a` with cached width
    | Text Int a

    | Line

    -- Lay out the first `Doc`, but when flattened (using `group`) - fall back to the second.
    -- This is used for implementing "flattened alternatives" (hence the name).
    --
    -- e.g.
    --
    -- ```
    -- line = FlatAlt Line space
    --
    -- group (FlatAlt _ y) = y
    -- render (FlatAlt x _) = x
    -- ```
    | FlatAlt (Doc a) (Doc a)

    -- Branching of possible layouts happens here!
    -- `Union` makes render 1st doc IIF there are enough width ELSE 2d doc
    --
    -- e.g.
    --
    -- ```
    -- softline = Union space Line
    -- softline = group line
    --
    -- group (Union x _) = x
    -- render (Union x y) = if enough_space then x else y
    -- ```
    --
    -- invariant: both documents should flatten to the same layout
    -- invariant: no first line in x is shorter than any first line in y
    | Union (Doc a) (Doc a)

    -- Reactive additions:
    | Column  (Int -> Doc a) -- see `column`
    | Nesting (Int -> Doc a) -- see `nesting`

derive instance functorDoc :: Functor Doc

instance semigroupDoc :: Semigroup (Doc a) where
  append Empty y = y
  append x Empty = x
  append x y = Cat x y

instance monoidDoc :: Monoid (Doc a) where
  mempty = Empty

emptyDoc :: forall a . Doc a
emptyDoc = Empty

-- | BASICS

-- | Construct a document from some `Renderable` fragment.
-- |
-- | The argument should not contain whitespace.
text :: forall a. Renderable a => a -> Doc a
text a = if l > 0 then Text l a else Empty
  where
  l = Renderable.width a

-- | `nest i x` lays out the document `x` with the current indentation level
-- | increased by `i`. Negative values are allowed and decrease the nesting level
-- | accordingly.
nest :: forall a. Int -> Doc a -> Doc a
nest 0 x = x -- Optimization
nest i x = Nest i x

-- | Lay the document out on a single line if it fits the available width.
group :: forall a. Renderable a => Doc a -> Doc a
group x = -- Union (flatten x) x
  case flattenMaybe x of
       Nothing -> x
       Just flattened -> Union flattened x

-- | `flatAlt x y` renders `x` by default, but falls back to `y` when grouped.
flatAlt ::
  forall a
   . Doc a -- default option
  -> Doc a -- grouped option
  -> Doc a
flatAlt = FlatAlt

-- | `flatAltFn f g` applys `f` to the default document and `g` to the grouped
-- | document.
flatAltFn ::
  forall a
   . (Doc a -> Doc a) -- apply to the default
  -> (Doc a -> Doc a) -- apply if grouped
  -> Doc a
  -> Doc a
flatAltFn f g x = FlatAlt (f x) (g x)

-- | The `line` document advances to the next line and indents to the current
-- | nesting level.
line :: forall a. Renderable a => Doc a
line = FlatAlt Line space

-- | `line'` is like `line`, but behaves like `mempty` if the line break
-- | is undone by `group` (instead of `space`).
line' :: forall a. Doc a
line' = FlatAlt Line Empty

-- | `softline` behaves like `space` if the resulting output fits the page,
-- otherwise like `line`.
softline :: forall a. Renderable a => Doc a
softline = Union space Line

-- | `softline'` is like `softline`, but behaves like `mempty` if the
-- | resulting output does not fit on the page (instead of `space`).
softline' :: forall a. Doc a
softline' = Union Empty Line

-- | A `hardline` is _always_ laid out as a line break, even when `group`ed or
-- | when there is plenty of space.
hardline :: forall a. Doc a
hardline = Line

-- | A single space document.
space :: forall a. Renderable a => Doc a
space = text Renderable.space

-- ALIGNMENT
-- These functions cannot be described by Wadler's original functions.
-- They align their output relative to the current output position - in
-- contrast to `nest` which always aligns to the current nesting level.
-- This deprives these functions from being _optimal_. In practice however
-- they prove to be very useful. Nonetheless they should be used with care,
-- since they are more expensive than the other functions.
-- | `align x` lays out the document x with the nesting level set to the
-- | current column.
align :: forall a. Doc a -> Doc a
align x =
  -- NOTE: nesting value could be negative
  column (\k -> nesting (\i -> nest (k - i) x))

-- | `hang i x` lays out the document `x` with the nesting level set to the
-- | _current column_ plus `i`. Negative values are allowed, and decrease the
-- | nesting level accordingly.
hang :: forall a. Int -> Doc a -> Doc a
hang i x = align (nest i x)

-- | `indent i x` indents document `x` with `i` spaces, starting from the
-- | current cursor position.
indent :: forall a. Renderable a => Int -> Doc a -> Doc a
indent i x = hang i (text (spaces i) <> x)

-- | Concatenate documents with a horizontal space between them.
hsep :: forall f a. Foldable f => Renderable a => f (Doc a) -> Doc a
hsep = intercalate space

-- | `vsep xs` concatenates all documents `xs` above each other. If a
-- | `group` undoes the line breaks inserted by `vsep`, the documents are
-- | separated with a space instead.
vsep :: forall f a. Foldable f => Renderable a => f (Doc a) -> Doc a
vsep = intercalate line

-- | `'fillSep' xs` concatenates the documents `xs` horizontally with `'<+>'`
-- as long as it fits the page, then inserts a `'line'` and continues doing that
-- for all documents in `xs`. (`'line'` means that if 'group'ed, the documents
-- are separated with a 'space' instead of newlines. Use 'fillCat' if you do not
-- want a 'space'.)
--
-- Let's print some words to fill the line:
--
-- >>> let docs = take 20 (cycle ["lorem", "ipsum", "dolor", "sit", "amet"])
-- >>> putDocW 80 ("Docs:" <+> fillSep docs)
-- Docs: lorem ipsum dolor sit amet lorem ipsum dolor sit amet lorem ipsum dolor
-- sit amet lorem ipsum dolor sit amet
--
-- The same document, printed at a width of only 40, yields
--
-- >>> putDocW 40 ("Docs:" <+> fillSep docs)
-- Docs: lorem ipsum dolor sit amet lorem
-- ipsum dolor sit amet lorem ipsum dolor
-- sit amet lorem ipsum dolor sit amet
fillSep :: forall a f . Container f => Foldable f => Renderable a => f (Doc a) -> Doc a
fillSep = concatWith (\x y -> x <> softline <> y)

-- | Concatenate all documents element-wise with a binary function.
--
-- @
-- 'concatWith' _ [] = 'mempty'
-- 'concatWith' (**) [x,y,z] = x ** y ** z
-- @
--
-- Multiple convenience definitions based on 'concatWith' are alredy predefined,
-- for example
--
-- @
-- 'hsep'    = 'concatWith' ('<+>')
-- 'fillSep' = 'concatWith' (\\x y -> x '<>' 'softline' '<>' y)
-- @
--
-- This is also useful to define customized joiners,
--
-- >>> concatWith (surround dot) ["Prettyprinter", "Render", "Text"]
-- Prettyprinter.Render.Text
concatWith :: forall a f . Container f => Foldable f => (Doc a -> Doc a -> Doc a) -> f (Doc a) -> Doc a
concatWith f =
  Container.uncons
    >>> case _ of
        Nothing -> Empty
        Just { head, tail } -> foldl f head tail

concatWithNonEmpty :: forall a . (Doc a -> Doc a -> Doc a) -> NonEmptyArray (Doc a) -> Doc a
concatWithNonEmpty f = NonEmptyArray.uncons >>> \{ head, tail } -> foldl f head tail

-- | `sep xs` tries laying out the documents `xs` separated with spaces,
-- | and if this does not fit the page, separates them with newlines. This is what
-- | differentiates it from `vsep`, which always lays out its contents beneath
-- | each other.
sep :: forall f a. Foldable f => Renderable a => f (Doc a) -> Doc a
sep = vsep >>> group

-- | `hcat xs` concatenates all documents horizontally with `append` (i.e.
-- | without any spacing).
hcat :: forall f a. Foldable f => f (Doc a) -> Doc a
hcat = fold

-- | `vcat xs` vertically concatenates all the documents. If it is grouped, the
-- | line breaks are removed.
vcat :: forall f a. Foldable f => f (Doc a) -> Doc a
vcat = intercalate line'

-- | @('fillCat' xs)@ concatenates documents @xs@ horizontally with @'<>'@ as
-- long as it fits the page, then inserts a @'line''@ and continues doing that
-- for all documents in @xs@. This is similar to how an ordinary word processor
-- lays out the text if you just keep typing after you hit the maximum line
-- length.
--
-- (@'line''@ means that if 'group'ed, the documents are separated with nothing
-- instead of newlines. See 'fillSep' if you want a 'space' instead.)
--
-- Observe the difference between 'fillSep' and 'fillCat'. 'fillSep'
-- concatenates the entries 'space'd when 'group'ed,
--
-- >>> let docs = take 20 (cycle (["lorem", "ipsum", "dolor", "sit", "amet"]))
-- >>> putDocW 40 ("Grouped:" <+> group (fillSep docs))
-- Grouped: lorem ipsum dolor sit amet
-- lorem ipsum dolor sit amet lorem ipsum
-- dolor sit amet lorem ipsum dolor sit
-- amet
--
-- On the other hand, 'fillCat' concatenates the entries directly when
-- 'group'ed,
--
-- >>> putDocW 40 ("Grouped:" <+> group (fillCat docs))
-- Grouped: loremipsumdolorsitametlorem
-- ipsumdolorsitametloremipsumdolorsitamet
-- loremipsumdolorsitamet
fillCat :: forall a . Array (Doc a) -> Doc a
fillCat = concatWith (\x y -> x <> softline' <> y)

-- | `cat xs` tries laying out the documents `xs` separated with nothing,
-- | and if this does not fit the page, separates them with newlines. This is what
-- | differentiates it from `vcat`, which always lays out its contents beneath
-- | each other.
cat :: forall f a. Foldable f => Renderable a => f (Doc a) -> Doc a
cat = vcat >>> group

-- REACTIVE LAYOUTS

-- | Layout a document depending on which column it starts at.
column :: forall a. (Int -> Doc a) -> Doc a
column = Column

-- | Layout a document depending on the current nesting level.
nesting :: forall a. (Int -> Doc a) -> Doc a
nesting = Nesting

-- | @('width' doc f)@ lays out the document 'doc', and makes the column width
-- of it available to a function.
--
-- >>> let annotate doc = width (brackets doc) (\w -> " <- width:" <+> pretty w)
-- >>> align (vsep (map annotate ["---", "------", indent 3 "---", vsep ["---", indent 4 "---"]]))
-- [---] <- width: 5
-- [------] <- width: 8
-- [   ---] <- width: 8
-- [---
--     ---] <- width: 8
width :: forall a . Doc a -> (Int -> Doc a) -> Doc a
width doc f
  = column (\colStart ->
        doc <> column (\colEnd ->
            f (colEnd - colStart)))

-- | @('fill' i x)@ lays out the document @x@. It then appends @space@s until
-- the width is equal to @i@. If the width of @x@ is already larger, nothing is
-- appended.
--
-- This function is quite useful in practice to output a list of bindings:
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fill 5 (pretty name) <+> "::" <+> pretty tp
-- >>> "let" <+> align (vcat (map ptype types))
-- let empty :: Doc
--     nest  :: Int -> Doc -> Doc
--     fillSep :: [Doc] -> Doc
fill
    :: forall a
     . Renderable a
    => Int -- ^ Append spaces until the document is at least this wide
    -> Doc a
    -> Doc a
fill n doc = width doc (\w -> text $ spaces (n - w))

-- | @('fillBreak' i x)@ first lays out the document @x@. It then appends @space@s
-- until the width is equal to @i@. If the width of @x@ is already larger than
-- @i@, the nesting level is increased by @i@ and a @line@ is appended. When we
-- redefine @ptype@ in the example given in 'fill' to use @'fillBreak'@, we get
-- a useful variation of the output:
--
-- >>> let types = [("empty","Doc"), ("nest","Int -> Doc -> Doc"), ("fillSep","[Doc] -> Doc")]
-- >>> let ptype (name, tp) = fillBreak 5 (pretty name) <+> "::" <+> pretty tp
-- >>> "let" <+> align (vcat (map ptype types))
-- let empty :: Doc
--     nest  :: Int -> Doc -> Doc
--     fillSep
--           :: [Doc] -> Doc
fillBreak
    :: forall a
     . Renderable a
    => Int -- ^ Append spaces until the document is at least this wide
    -> Doc a
    -> Doc a
fillBreak f x = width x (\w ->
    if w > f
        then nest f line'
        else text $ spaces (f - w))

-- OPERATORS

-- | Append two documents with a space between them.
surroundWithSpace :: forall a. Renderable a => Doc a -> Doc a -> Doc a
surroundWithSpace = surround space

infixr 5 surroundWithSpace as <+>

-- CONVENIENCE

-- $
-- prop> \(NonNegative n) -> length (show (spaces n)) == n
--
-- >>> case spaces 1 of Char ' ' -> True; _ -> False
-- True
--
-- >>> case spaces 0 of Empty -> True; _ -> False
-- True
--
-- prop> \(Positive n) -> case (spaces (-n)) of Empty -> True; _ -> False



-- | @('plural' n one many)@ is @one@ if @n@ is @1@, and @many@ otherwise. A
-- typical use case is  adding a plural "s".
--
-- >>> let things = [True]
-- >>> let amount = length things
-- >>> pretty things <+> "has" <+> pretty amount <+> plural "entry" "entries" amount
-- [True] has 1 entry
plural
    :: forall doc
     . doc -- ^ @1@ case
    -> doc -- ^ other cases
    -> Int
    -> doc
plural one multiple n
    | n == 1    = one
    | otherwise = multiple

-- | `enclose' l r x` encloses document `x` between documents `l` and `r`
-- using `append`.
enclose :: forall a. Doc a -> Doc a -> Doc a -> Doc a
enclose l r x = l <> x <> r

-- | @('surround' x l r)@ surrounds document @x@ with @l@ and @r@.
--
-- >>> surround "·" "A" "Z"
-- A·Z
--
-- This is merely an argument reordering of @'enclose'@, but allows for
-- definitions like
--
-- >>> concatWith (surround dot) ["Prettyprinter", "Render", "Text"]
-- Prettyprinter.Render.Text
surround
    :: forall a
     . Doc a
    -> Doc a
    -> Doc a
    -> Doc a
surround x l r = l <> x <> r

-- | `punctuate p xs` appends `p` to all but the last document in `xs`.
punctuate ::
  forall f a.
  Container f =>
  Functor f =>
  Doc a ->
  f (Doc a) ->
  f (Doc a)
punctuate p = Container.mapInit (_ <> p)

-- | `punctuate' p xs` prepends `p` to all but the first document in `xs`.
punctuate' ::
  forall f a.
  Container f =>
  Functor f =>
  Doc a ->
  f (Doc a) ->
  f (Doc a)
punctuate' p = Container.mapTail (p <> _)

-- RENDERING
-- | Render a document, trying not to exceed a maximum line width.
render :: forall a. Renderable a => Int -> Doc a -> a
render w doc = layout $ forceSimpleDocStream $ best w 0 $ (Tuple 0 doc) : Nil

-- INTERNALS
flatten :: forall a. Doc a -> Doc a
flatten Empty = Empty
flatten Fail = Fail
flatten (Cat x y) = Cat (flatten x) (flatten y)
flatten (Nest i x) = flatten x
flatten (Column f) = Column (f >>> flatten)
flatten (Nesting f) = Nesting (f >>> flatten)
flatten (Text l a) = Text l a
flatten Line = Fail
flatten (FlatAlt _ y) = flatten y
flatten (Union x _) = flatten x -- important

-- | Returns `Nothing` if flattening has no effect. Useful as an optimization
-- | in `group`.
flattenMaybe :: forall a. Doc a -> Maybe (Doc a)
flattenMaybe Empty = Nothing
flattenMaybe Fail = Nothing
flattenMaybe (Nest i x) = Nest i <$> flattenMaybe x
flattenMaybe (Column f) = Just (Column (f >>> flatten))
flattenMaybe (Nesting f) = Just (Nesting (f >>> flatten))
flattenMaybe (Text _ _) = Nothing
flattenMaybe Line = Just Fail
flattenMaybe (FlatAlt _ y) = Just (flatten y)
flattenMaybe (Union x _) = flattenMaybe x <|> Just x
flattenMaybe (Cat x y) = case flattenMaybe x, flattenMaybe y of
  Nothing, Nothing -> Nothing
  Just x', Nothing -> Just (Cat x' y)
  Nothing, Just y' -> Just (Cat x y')
  Just x', Just y' -> Just (Cat x' y')

-- | List of indentation/document pairs.
type Docs a
  = List (Tuple Int (Doc a))

-- | A List/stream of document fragments ready for layout. Think of this as a
-- | single "path" through a document.
data SimpleDocStream a
  = SFail -- a stream ending in `SFail` doesn't _fit_
  | SEmpty
  | SText Int a (SimpleDocStream a)
  | SLine Int (SimpleDocStream a)

data LazySimpleDocStream a
  = LSFail -- a stream ending in `LSFail` doesn't _fit_
  | LSEmpty
  | LSText Int a (Lazy (LazySimpleDocStream a))
  | LSLine Int (Lazy (LazySimpleDocStream a))

forceSimpleDocStream :: forall a. LazySimpleDocStream a -> SimpleDocStream a
forceSimpleDocStream = case _ of
  LSFail -> SFail
  LSEmpty -> SEmpty
  LSText i s x -> SText i s (forceSimpleDocStream $ Lazy.force x)
  LSLine i x -> SLine i (forceSimpleDocStream $ Lazy.force x)

-- | Actual render a chosen document stream.
layout :: forall a. Renderable a => SimpleDocStream a -> a
layout SFail = unsafeCrashWith "attempt to layout SFail" -- shouldn't happen!
layout SEmpty = mempty
layout (SText _ a x) = a <> layout x
layout (SLine i x) = Renderable.newline <> spaces i <> layout x

-- | Select the "best" layout stream/path from a document tree.
best ::
  forall a.
  Int -> -- available width
  Int -> -- column number (i.e. chars on this line, including indentation)
  Docs a ->
  LazySimpleDocStream a
best w k = case _ of
  Nil -> LSEmpty
  ((Tuple identation doc) : rest) -> case doc of
    Empty -> best w k rest
    Fail -> LSFail
    Text l a -> LSText l a (Lazy.defer \_ -> best w (k + l) rest)
    Line -> LSLine identation (Lazy.defer \_ -> best w identation rest)
    Cat x y -> best w k (Tuple identation x : Tuple identation y : rest)
    Nest j x -> best w k (Tuple (identation + j) x : rest)
    Column f -> best w k (Tuple identation (f k) : rest)
    Nesting f -> best w k (Tuple identation (f identation) : rest)
    FlatAlt x _ -> best w k (Tuple identation x : rest)
    Union x y ->
      better w k
        (best w k (Tuple identation x : rest))
        (best w k (Tuple identation y : rest))

better ::
  forall a.
  Int ->
  Int ->
  LazySimpleDocStream a ->
  LazySimpleDocStream a ->
  LazySimpleDocStream a
better w k x y = if fits (w - k) x then x else y

fits :: forall a. Int -> LazySimpleDocStream a -> Boolean
fits w _
  | w < 0 = false
fits _ LSFail = false -- NOTE: This is what Fail constructors are for!
fits _ LSEmpty = true
fits w (LSText l _ x) = fits (w - l) (Lazy.force x)
fits _ (LSLine _ _) = true

spaces :: forall a. Renderable a => Int -> a
spaces n = if n > 0 then copy n Renderable.space else mempty

-- | Used for indentation
copy :: forall a. Monoid a => Int -> a -> a
copy n a = fold (replicate n a :: Array a)

-- | Everyone's fav Haskell function.
unsafeCrashWith :: forall a. String -> a
unsafeCrashWith msg = unsafePartial (crashWith msg)

-- | `Doc a` is not a monoid in respect to FlatAlt, it is a monoid for Empty only
-- | that's why we need to jump extra hoops if we want to omit Empty
-- |
-- | It's preffereably to use `concatWith (surroundOmittingEmpty line')` instead of vcat
-- |
-- | ```purs
-- | -- having
-- | line' = FlatAlt Line Empty
-- |
-- | -- For example:
-- |
-- | -- is using monoid inside
-- | vcat [Empty, Empty, Empty] == intercalate line' [Empty, Empty, Empty] == concatWith (surround line') [Empty, Empty, Empty] "\n\n"
-- |
-- | -- but this works!
-- | concatWith (surroundOmittingEmpty line') [Empty, Empty, Empty] => ""
-- | ```

surroundOmittingEmpty :: forall a . Doc a -> Doc a -> Doc a -> Doc a
surroundOmittingEmpty _ Empty y        = y
surroundOmittingEmpty _ x Empty        = x
surroundOmittingEmpty textInMiddle x y = x <> textInMiddle <> y

vcatOmittingEmpty :: forall f a. Container f => Foldable f => Renderable a => f (Doc a) -> Doc a
vcatOmittingEmpty = concatWith (surroundOmittingEmpty line')

vcatOmittingEmptyNonEmpty :: forall a . Renderable a => NonEmptyArray (Doc a) -> Doc a
vcatOmittingEmptyNonEmpty = concatWithNonEmpty (surroundOmittingEmpty line')
