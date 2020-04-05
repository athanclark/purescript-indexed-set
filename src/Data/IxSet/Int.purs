module Data.IxSet.Int
  ( increment, new, decodeJsonIxSet, fromArray, fromObject
  , module Ix
  ) where

import Data.IxSet
  ( Index, IxSet
  , insert, insertMany, delete, lookup, toArray, toObject
  ) as Ix
import Data.IxSet (IxSet, Index (..))
import Data.IxSet (new, decodeJsonIxSet, fromArray, fromObject) as IxUnpub

import Prelude
import Data.Either (Either)
import Data.Argonaut (class DecodeJson, Json)
import Foreign.Object (Object)
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, modify) as Ref


increment :: Ref Int -> Effect Index
increment nRef = Index <<< show <$> Ref.modify (_ + 1) nRef


-- FIXME make the EncodeArrayBuffer and DecodeArrayBuffer instance for this variant rely on i32BE instead of String
decodeJsonIxSet :: forall a. DecodeJson a => Json -> Either String (Effect {set :: IxSet a, indicies :: Array Index})
decodeJsonIxSet =
  IxUnpub.decodeJsonIxSet new


-- | Uses a "nonce" as an index, just by incrementing an `Int`. Should not be used
-- | for long-living sets, or ones that may have over `2^32` elements (over time).
new :: forall a. Effect (Ix.IxSet a)
new = do
  nRef <- Ref.new 0
  IxUnpub.new (increment nRef)


fromArray :: forall a. Array a -> Effect {set :: IxSet a, indicies :: Array Index}
fromArray =
  IxUnpub.fromArray new


fromObject :: forall a. Object a -> Effect (IxSet a)
fromObject o = do
  nRef <- Ref.new 0
  IxUnpub.fromObject (increment nRef) o
