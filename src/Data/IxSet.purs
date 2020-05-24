module Data.IxSet where

import Data.IntMap (IntMap)
import Data.IntMap (values, lookup, insert, delete, empty, toUnfoldable) as IntMap

import Prelude
import Data.Maybe (Maybe (..))
import Data.Tuple (Tuple)
import Data.Array (sort, toUnfoldable, snoc) as Array
import Data.Unfoldable (class Unfoldable)
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class
  ( class DynamicByteLength, class EncodeArrayBuffer, class DecodeArrayBuffer
  , byteLength, putArrayBuffer, readArrayBuffer)
import Data.Traversable (class Traversable, traverse, sequence)
import Data.Set (fromFoldable) as Set
import Test.QuickCheck (class Arbitrary, arbitrary)


type Index = Int

-- | Is intended to obfuscate all `Index`es to their values - serialized and foldable versions
-- | are simply represented as arrays.
newtype IxSet a = IxSet
  { mapping   :: IntMap a
  , nextIndex :: Int
  }
derive instance genericIxSet :: Generic a a' => Generic (IxSet a) _
instance eqIxSet :: Ord a => Eq (IxSet a) where
  eq (IxSet {mapping: map1}) (IxSet {mapping: map2}) =
    let x :: Array _
        x = IntMap.values map1
        y :: Array _
        y = IntMap.values map2
    in  (Array.sort x) == (Array.sort y)
-- | Relies on Set ordering to compare values
instance ordIxSet :: Ord a => Ord (IxSet a) where
  compare x y =
    let xs :: Array a
        xs = toUnfoldable' x
        ys :: Array a
        ys = toUnfoldable' y
    in  compare (Set.fromFoldable xs) (Set.fromFoldable ys)
instance showIxSet :: (Show a, Ord a) => Show (IxSet a) where
  show set = show (Array.sort (toUnfoldable' set))
instance functorIxSet :: Functor IxSet where
  map f (IxSet x) = IxSet x { mapping = map f x.mapping }
instance foldableIxSet :: Foldable IxSet where
  foldMap f (IxSet {mapping}) = foldMap f mapping
  foldr f acc (IxSet {mapping}) = foldr f acc mapping
  foldl f acc (IxSet {mapping}) = foldl f acc mapping
instance traversableIxSet :: Traversable IxSet where
  traverse f (IxSet x) = (\mapping -> IxSet x {mapping = mapping}) <$> traverse f x.mapping
  sequence (IxSet x) = (\mapping -> IxSet x {mapping = mapping}) <$> sequence x.mapping
-- | Encodes to an Array
instance encodeJsonIxSet :: EncodeJson a => EncodeJson (IxSet a) where
  encodeJson set =
    let xs :: Array a
        xs = toUnfoldable' set
    in  encodeJson xs
instance decodeJsonIxSet :: DecodeJson a => DecodeJson (IxSet a) where
  decodeJson json = do
    (xs :: Array a) <- decodeJson json
    let {set} = fromFoldable xs
    pure set
instance dynamicByteLengthIxSet :: DynamicByteLength a => DynamicByteLength (IxSet a) where
  byteLength set =
    let xs :: Array a
        xs = toUnfoldable' set
    in  byteLength xs
instance encodeArrayBufferIxSet :: EncodeArrayBuffer a => EncodeArrayBuffer (IxSet a) where
  putArrayBuffer b o set =
    let xs :: Array a
        xs = toUnfoldable' set
    in  putArrayBuffer b o xs
instance decodeArrayBufferIxSet :: (DynamicByteLength a, DecodeArrayBuffer a) => DecodeArrayBuffer (IxSet a) where
  readArrayBuffer b o = do
    (mxs :: Maybe (Array a)) <- readArrayBuffer b o
    case mxs of
      Nothing -> pure Nothing
      Just xs ->
        let {set} = fromFoldable xs
        in  pure (Just set)
instance arbitraryIxSet :: Arbitrary a => Arbitrary (IxSet a) where
  arbitrary = do
    (xs :: Array a) <- arbitrary
    let {set} = fromFoldable xs
    pure set


empty :: forall a. IxSet a
empty = IxSet {mapping: IntMap.empty, nextIndex: 0}


insert :: forall a. a -> IxSet a -> {set :: IxSet a, index :: Index}
insert x (IxSet {mapping, nextIndex}) =
  { set: IxSet
    { mapping: IntMap.insert nextIndex x mapping
    , nextIndex: nextIndex + 1
    }
  , index: nextIndex
  }


delete :: forall a. Index -> IxSet a -> IxSet a
delete i (IxSet x) = IxSet x {mapping = IntMap.delete i x.mapping}


lookup :: forall a. Index -> IxSet a -> Maybe a
lookup i (IxSet {mapping}) = IntMap.lookup i mapping


fromFoldable :: forall a f. Foldable f => f a -> {set :: IxSet a, indicies :: Array Index}
fromFoldable xs = foldr go {set: empty, indicies: []} xs
  where
    go x {set,indicies} =
      let {set: set', index} = insert x set
      in  {set: set', indicies: Array.snoc indicies index}


toUnfoldable :: forall a f. Unfoldable f => IxSet a -> f (Tuple Index a)
toUnfoldable (IxSet {mapping}) = IntMap.toUnfoldable mapping


toUnfoldable' :: forall a f. Unfoldable f => IxSet a -> f a
toUnfoldable' (IxSet {mapping}) =
  Array.toUnfoldable (IntMap.values mapping)
