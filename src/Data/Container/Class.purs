module Data.Container.Class
  ( class Container
  , cons
  , uncons
  , snoc
  , unsnoc
  , mapTail
  , mapInit
  ) where

import Prelude
import Data.Array as Array
import Data.List as List
import Data.Maybe (Maybe(Just, Nothing))

class Container f where
  cons :: forall a. a -> f a -> f a
  uncons :: forall a. f a -> Maybe { head :: a, tail :: f a }
  snoc :: forall a. f a -> a -> f a
  unsnoc :: forall a. f a -> Maybe { init :: f a, last :: a }

instance containerList :: Container List.List where
  cons = List.Cons
  uncons = List.uncons
  snoc = List.snoc
  unsnoc = List.unsnoc

instance containerArray :: Container Array where
  cons = Array.cons
  uncons = Array.uncons
  snoc = Array.snoc
  unsnoc = Array.unsnoc

mapTail :: forall f a. Container f => Functor f => (a -> a) -> f a -> f a
mapTail f x = case uncons x of
  Nothing -> x
  Just { head, tail } -> cons head (map f tail)

mapInit :: forall f a. Container f => Functor f => (a -> a) -> f a -> f a
mapInit f x = case unsnoc x of
  Nothing -> x
  Just { init, last } -> snoc (map f init) last
