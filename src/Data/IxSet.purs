module Data.IxSet
  ( Index (..), IxSet, decodeJsonIxSet
  , new, insert, insertMany, delete, lookup, fromArray, toArray
  ) where

import Prelude
import Data.Maybe (Maybe)
import Data.Either (Either)
import Data.Generic.Rep (class Generic)
import Data.Argonaut (Json, class EncodeJson, class DecodeJson, encodeJson, decodeJson)
import Data.ArrayBuffer.Class (class EncodeArrayBuffer, class DecodeArrayBuffer)
import Data.Vec (Vec)
import Data.Traversable (traverse)
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
  { genIx   :: a -> Effect Index
  , valsRef :: Ref (Object a)
  }

instance encodeJsonIxSet :: EncodeJson a => EncodeJson (IxSet a) where
  encodeJson set =
    encodeJson (unsafePerformEffect (toArray set))

decodeJsonIxSet :: forall a. DecodeJson a => Effect (IxSet a) -> Json -> Either String (Effect {set :: IxSet a, indicies :: Array Index})
decodeJsonIxSet newIxSet json = do
  xs <- decodeJson json
  pure (fromArray newIxSet xs)


new :: forall a. (a -> Effect Index) -> Effect (IxSet a)
new genIx = do
  valsRef <- Ref.new Obj.empty
  pure (IxSet {genIx, valsRef})


insert :: forall a. a -> IxSet a -> Effect Index
insert x (IxSet {genIx, valsRef}) = do
  ix'@(Index ix) <- genIx x
  Ref.modify_ (Obj.insert ix x) valsRef
  pure ix'


insertMany :: forall a n. Vec n a -> IxSet a -> Effect (Vec n Index)
insertMany xs set = traverse (flip insert set) xs


delete :: forall a. Index -> IxSet a -> Effect Unit
delete (Index ix) (IxSet {valsRef}) =
  Ref.modify_ (Obj.delete ix) valsRef


lookup :: forall a. Index -> IxSet a -> Effect (Maybe a)
lookup (Index ix) (IxSet {valsRef}) = do
  vals <- Ref.read valsRef
  pure (Obj.lookup ix vals)


-- | Given some array of elements (stored as an Array), build a indexed set and report their indicies.
fromArray :: forall a. Effect (IxSet a) -> Array a -> Effect {set :: IxSet a, indicies :: Array Index}
fromArray newIxSet xs = do
  set <- newIxSet
  indicies <- traverse (flip insert set) xs
  pure {set, indicies}


toArray :: forall a. IxSet a -> Effect (Array a)
toArray (IxSet {valsRef}) = Obj.toArrayWithKey (flip const) <$> Ref.read valsRef
