module Data.IxSet
  ( Index (..), IxSet
  , new, insert, delete, lookup
  ) where

import Prelude
import Data.Maybe (Maybe)
import Foreign.Object (Object)
import Foreign.Object (empty, insert, delete, lookup) as Obj
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref (new, modify_, read) as Ref


newtype Index = Index String

newtype IxSet a = IxSet
  { genIx :: a -> Effect Index
  , valsRef :: Ref (Object a)
  }


new :: forall a. (a -> Effect Index) -> Effect (IxSet a)
new genIx = do
  valsRef <- Ref.new Obj.empty
  pure (IxSet {genIx, valsRef})


insert :: forall a. a -> IxSet a -> Effect Index
insert x (IxSet {genIx, valsRef}) = do
  ix'@(Index ix) <- genIx x
  Ref.modify_ (Obj.insert ix x) valsRef
  pure ix'


delete :: forall a. Index -> IxSet a -> Effect Unit
delete (Index ix) (IxSet {valsRef}) =
  Ref.modify_ (Obj.delete ix) valsRef


lookup :: forall a. Index -> IxSet a -> Effect (Maybe a)
lookup (Index ix) (IxSet {valsRef}) = do
  vals <- Ref.read valsRef
  pure (Obj.lookup ix vals)
