{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Miso.IndexDb.List where

import Data.Type.Bool ( If )
import Data.Type.Equality (type (==))
import Miso.IndexDb.Prelude ( Bool(..), Symbol )

type family Drop (name :: Symbol) (xs :: [Symbol]) :: [Symbol] where
  Drop n '[]       = '[]
  Drop n (x ': xs) = DropStep (n == x) n x xs

type family DropStep
  (same :: Bool)
  (n :: Symbol)
  (x :: Symbol)
  (xs :: [Symbol]) ::
  [Symbol] where
  DropStep 'True  n x xs = Drop n xs
  DropStep 'False n x xs = x ': Drop n xs

type family Elem (name :: Symbol) (xs :: [Symbol]) :: Bool where
  Elem n '[]       = 'False
  Elem n (x ': xs) = ElemStep (n == x) n xs

type family ElemStep (same :: Bool) (n :: Symbol) (xs :: [Symbol]) :: Bool where
  ElemStep 'True  n xs = 'True
  ElemStep 'False n xs = Elem n xs

type family FilterOut x l where
  FilterOut x '[] = '[]
  FilterOut x (h:t) = If (x == h) (FilterOut x t) (h : FilterOut x t)

type family Diff l1 l2 where
  Diff l1 '[] = l1
  Diff '[] _ = '[]
  Diff t1 (h2:t2) = Diff (FilterOut h2 t1) t2
