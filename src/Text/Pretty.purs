module Text.Pretty
    ( Doc

    -- Basic functionality
    , text, nest, group, flatAlt, flatAltFn
    , line, line', softline, softline', hardline
    , space

    -- Alignment
    , align, hang, indent

    -- Folds
    , hsep, vsep, sep  -- replace newlines with spaces when grouped
    , hcat, vcat, cat  -- remove newlines when grouped

    -- Reactive/conditional layouts
    , column, nesting

    -- Operators
    , appendWithSpace, (<+>)

    -- General convenience
    , enclose, punctuate, punctuate'

    -- Render
    , render
    )
where

-- NOTE: Think of and build your layout in its narrowest form, then `group` the
-- parts that _could_ be flattened if there's space available.

import Prelude

import Control.Alt ((<|>))
import Data.Container.Class (class Container)
import Data.Container.Class as Container
import Data.Foldable (class Foldable, fold, intercalate)
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

    -- Lay out the first `Doc`, but when flattened fall back to the second.
    -- This is used for implementing "flattened alternatives" (hence the name).
    -- e.g. `line` vs `line'`
    | FlatAlt (Doc a) (Doc a)

    -- Branching of possible layouts happens here!
    -- invariant: both documents should flatten to the same layout
    -- invariant: no first line in x is shorter than any first line in y
    | Union (Doc a) (Doc a)

    -- Reactive additions:
    | Column  (Int -> Doc a) -- see `column`
    | Nesting (Int -> Doc a) -- see `nesting`


derive instance functorDoc :: Functor Doc

instance semigroupDoc :: Semigroup (Doc a) where
    append Empty y     = y
    append x     Empty = x
    append x     y     = Cat x y

instance monoidDoc :: Monoid (Doc a) where
    mempty = Empty


-- BASICS


-- | Construct a document from some `Renderable` fragment.
-- |
-- | The argument should not contain whitespace.
text :: forall a. Renderable a => a -> Doc a
text a = if l > 0 then Text l a else mempty
    where l = Renderable.width a


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
flatAlt
    :: forall a
     . Doc a -- default option
    -> Doc a -- grouped option
    -> Doc a
flatAlt = FlatAlt


-- | `flatAltFn f g` applys `f` to the default document and `g` to the grouped
-- | document.
flatAltFn
    :: forall a
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
line' = FlatAlt Line mempty


-- | `softline` behaves like `space` if the resulting output fits the page,
-- otherwise like `line`.
softline :: forall a. Renderable a => Doc a
softline = Union space Line


-- | `softline'` is like `softline`, but behaves like `mempty` if the
-- | resulting output does not fit on the page (instead of `space`).
softline' :: forall a. Renderable a => Doc a
softline' = Union mempty Line


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


-- | Layout a documnet depending on the current nesting level.
nesting :: forall a. (Int -> Doc a) -> Doc a
nesting = Nesting


-- OPERATORS


-- | Append two documents with a space between them.
appendWithSpace :: forall a. Renderable a => Doc a -> Doc a -> Doc a
appendWithSpace = appendWith space


infixr 5 appendWithSpace as <+>


-- CONVENIENCE


-- | `enclose' l r x` encloses document `x` between documents `l` and `r`
-- using `append`.
enclose :: forall a. Doc a -> Doc a -> Doc a -> Doc a
enclose l r x = l <> x <> r


-- | `punctuate p xs` appends `p` to all but the last document in `xs`.
punctuate
    :: forall f a. Container f => Functor f => Renderable a
    => Doc a -> f (Doc a)
    -> f (Doc a)
punctuate p = Container.mapInit (_ <> p)


-- | `punctuate' p xs` prepends `p` to all but the first document in `xs`.
punctuate'
    :: forall f a. Container f => Functor f => Renderable a
    => Doc a -> f (Doc a)
    -> f (Doc a)
punctuate' p = Container.mapTail (p <> _)


-- RENDERING


-- | Render a document, trying not to exceed a maximum line width.
render :: forall a. Renderable a => Int -> Doc a -> a
render width doc = layout $ forceSimpleDocStream $ best width 0 $ (Tuple 0 doc) : Nil


-- INTERNALS


flatten :: forall a. Renderable a => Doc a -> Doc a
flatten Empty         = Empty
flatten Fail          = Fail
flatten (Cat x y)     = Cat (flatten x) (flatten y)
flatten (Nest i x)    = flatten x
flatten (Column f)    = Column (f >>> flatten)
flatten (Nesting f)   = Nesting (f >>> flatten)
flatten (Text l a)    = Text l a
flatten Line          = Fail
flatten (FlatAlt _ y) = flatten y
flatten (Union x _)   = flatten x -- important


-- | Returns `Nothing` if flattening has no effect. Useful as an optimization
-- | in `group`.
flattenMaybe :: forall a. Renderable a => Doc a -> Maybe (Doc a)
flattenMaybe Empty         = Nothing
flattenMaybe Fail          = Nothing
flattenMaybe (Nest i x)    = Nest i <$> flattenMaybe x
flattenMaybe (Column f)    = Just (Column (f >>> flatten))
flattenMaybe (Nesting f)   = Just (Nesting (f >>> flatten))
flattenMaybe (Text _ _)    = Nothing
flattenMaybe Line          = Just Fail
flattenMaybe (FlatAlt _ y) = Just (flatten y)
flattenMaybe (Union x _)   = flattenMaybe x <|> Just x
flattenMaybe (Cat x y)     =
    case flattenMaybe x, flattenMaybe y of
         Nothing, Nothing -> Nothing
         Just x', Nothing -> Just (Cat x' y)
         Nothing, Just y' -> Just (Cat x y')
         Just x', Just y' -> Just (Cat x' y')


-- | List of indentation/document pairs.
type Docs a = List (Tuple Int (Doc a))


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

forceSimpleDocStream :: forall a . LazySimpleDocStream a -> SimpleDocStream a
forceSimpleDocStream = case _ of
  LSFail -> SFail
  LSEmpty -> SEmpty
  LSText i s x -> SText i s (forceSimpleDocStream $ Lazy.force x)
  LSLine i x -> SLine i (forceSimpleDocStream $ Lazy.force x)

-- | Actual render a chosen document stream.
layout :: forall a. Renderable a => SimpleDocStream a -> a
layout SFail         = error "attempt to layout SFail" -- shouldn't happen!
layout SEmpty        = mempty
layout (SText _ a x) = a <> layout x
layout (SLine i x)   = Renderable.newline <> spaces i <> layout x


-- | Select the "best" layout stream/path from a document tree.
best
    :: forall a
     . Renderable a
    => Int -- available width
    -> Int -- column number (i.e. chars on this line, including indentation)
    -> Docs a
    -> LazySimpleDocStream a
best w k = case _ of
  Nil -> LSEmpty
  ((Tuple identation doc) : rest) ->
    case doc of
      Empty       -> best w k rest
      Fail        -> LSFail
      Text l a    -> LSText l a (Lazy.defer \_ -> best w (k + l) rest)
      Line        -> LSLine identation (Lazy.defer \_ ->  best w identation rest)
      Cat x y     -> best w k (Tuple identation x : Tuple identation y : rest)
      Nest j x    -> best w k (Tuple (identation + j) x : rest)
      Column f    -> best w k (Tuple identation (f k) : rest)
      Nesting f   -> best w k (Tuple identation (f identation) : rest)
      FlatAlt x y -> best w k (Tuple identation x : rest)
      Union x y   ->
          better w k
              (best w k (Tuple identation x : rest))
              (best w k (Tuple identation y : rest))

better
    :: forall a
     . Int
    -> Int
    -> LazySimpleDocStream a
    -> LazySimpleDocStream a
    -> LazySimpleDocStream a
better w k x y = if fits (w - k) x then x else y


fits :: forall a. Int -> LazySimpleDocStream a -> Boolean
fits w _ | w < 0     = false
fits _ LSFail         = false  -- NOTE: This is what Fail constructors are for!
fits _ LSEmpty        = true
fits w (LSText l _ x) = fits (w - l) (Lazy.force x)
fits _ (LSLine _ _)   = true


-- | Append documents with some other document between them.
appendWith :: forall a. Doc a -> Doc a -> Doc a -> Doc a
appendWith s x y = x <> s <> y


spaces :: forall a. Renderable a => Int -> a
spaces n = if n > 0 then copy n Renderable.space else mempty


-- | Used for indentation
copy :: forall a. Monoid a => Int -> a -> a
copy n a = fold (replicate n a :: Array a)


-- | Everyone's fav Haskell function.
error :: forall a. String -> a
error msg = unsafePartial (crashWith msg)
