module Data.IxSet
  ( Index (..), IxSet, decodeJsonIxSet
  , new, insert, insertMany, delete, lookup, fromArray, toArray, fromObject, toObject
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (Json, class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer)
import Data.Vec (Vec)
import Data.Traversable (traverse)
import Data.Set (fromFoldable) as Set
import Foreign.Object (Object)
import Foreign.Object (empty, insert, delete, lookup, toArrayWithKey) as Obj
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, modify_, read) as Ref
import Effect.Unsafe (unsafePerformEffect)


newtype Index = Index String
derive instance genericIndex :: Generic Index _
derive newtype instance eqIndex :: Eq Index
derive newtype instance showIndex :: Show Index
derive newtype instance encodeJsonIndex :: EncodeJson Index
derive newtype instance decodeJsonIndex :: DecodeJson Index
derive newtype instance encodeArrayBufferIndex :: EncodeArrayBuffer Index
derive newtype instance decodeArrayBufferIndex :: DecodeArrayBuffer Index

newtype IxSet a = IxSet
  { genIx   :: Effect Index
  , valsRef :: Ref (Object a)
  }
derive instance genericIxSet :: Generic a a' => Generic (IxSet a) _
-- | Encodes to an Array
instance encodeJsonIxSet :: EncodeJson a => EncodeJson (IxSet a) where
  encodeJson set =
    encodeJson (unsafePerformEffect (toArray set))
-- | Relies on Set ordering to compare values
instance eqIxSet :: Ord a => Eq (IxSet a) where
  eq x y = unsafePerformEffect do
    xs <- toArray x
    ys <- toArray y
    pure (Set.fromFoldable xs == Set.fromFoldable ys)
instance ordIxSet :: Ord a => Ord (IxSet a) where
  compare x y = unsafePerformEffect do
    xs <- toArray x
    ys <- toArray y
    pure (compare (Set.fromFoldable xs) (Set.fromFoldable ys))
instance functorIxSet :: Functor IxSet where
  map f (IxSet {valsRef,genIx}) = unsafePerformEffect do
    xs <- Ref.read valsRef
    newValsRef <- Ref.new (map f xs)
    pure (IxSet {valsRef: newValsRef, genIx})

decodeJsonIxSet :: forall a. DecodeJson a => Effect (IxSet a) -> Json -> Either String (Effect {set :: IxSet a, indicies :: Array Index})
decodeJsonIxSet newIxSet json = do
  xs <- decodeJson json
  pure (fromArray newIxSet xs)


new :: forall a. Effect Index -> Effect (IxSet a)
new genIx = do
  valsRef <- Ref.new Obj.empty
  pure (IxSet {genIx, valsRef})


insert :: forall a. a -> IxSet a -> Effect Index
insert x (IxSet {genIx, valsRef}) = do
  ix'@(Index ix) <- genIx
  Ref.modify_ (Obj.insert ix x) valsRef
  pure ix'


insertMany :: forall a n. Vec n a -> IxSet a -> Effect (Vec n Index)
insertMany xs set =
  traverse (flip insert set) xs


delete :: forall a. Index -> IxSet a -> Effect Unit
delete (Index ix) (IxSet {valsRef}) =
  Ref.modify_ (Obj.delete ix) valsRef


lookup :: forall a. Index -> IxSet a -> Effect (Maybe a)
lookup (Index ix) set =
  Obj.lookup ix <$> toObject set


-- | Given some array of elements (stored as an Array), build a indexed set and report their indicies.
fromArray :: forall a. Effect (IxSet a) -> Array a -> Effect {set :: IxSet a, indicies :: Array Index}
fromArray newIxSet xs = do
  set <- newIxSet
  indicies <- traverse (flip insert set) xs
  pure {set, indicies}


toArray :: forall a. IxSet a -> Effect (Array a)
toArray set =
  Obj.toArrayWithKey (flip const) <$> toObject set


-- | Backdoor to build a set with index & value pairs
fromObject :: forall a. Effect Index -> Object a -> Effect (IxSet a)
fromObject genIx obj = do
  valsRef <- Ref.new obj
  pure (IxSet {valsRef, genIx})


-- | Backdoor to see every index & value
toObject :: forall a. IxSet a -> Effect (Object a)
toObject (IxSet {valsRef}) =
  Ref.read valsRef
