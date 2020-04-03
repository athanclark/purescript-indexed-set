module Data.IxSet.Int
  ( module Ix
  ) where

import Data.IxSet (Index, IxSet, new, insert, delete, lookup) as Ix
import Data.IxSet (Index (..))

import Prelude
import Effect (Effect)
import Effect.Ref (new, modify) as Ref


-- | Uses a "nonce" as an index, just by incrementing an `Int`. Should not be used
-- | for long-living sets, or ones that may have over `2^32` elements (over time).
new :: forall a. Effect (Ix.IxSet a)
new = do
  nRef <- Ref.new 0
  Ix.new (\_ -> Index <<< show <$> Ref.modify (_ + 1) nRef)
