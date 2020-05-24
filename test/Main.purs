module Test.Main where

import Data.IxSet (IxSet, Index)
import Data.IxSet (insert, delete, lookup, empty) as IxSet

import Prelude
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Array (snoc) as Array
import Data.Foldable (foldr)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Class.Console (log)
import Test.QuickCheck (quickCheckGen, arbitrary, Result (..))
import Test.QuickCheck.Gen (Gen, arrayOf)
import Data.Argonaut (encodeJson, decodeJson)
import Data.ArrayBuffer.Class (encodeArrayBuffer, decodeArrayBuffer)
import Data.ArrayBuffer.Class.Types (Int32BE (..))


main :: Effect Unit
main = do
  log "IxSet"
  log " - insert exists"
  quickCheckGen insertExistsIxSet
  log " - delete doesn't exist"
  quickCheckGen deleteDoesntExistIxSet
  log " - json iso"
  quickCheckGen jsonIsoIxSet
  log " - arraybuffer iso"
  quickCheckGen abIsoIxSet



insertExistsIxSet :: Gen Result
insertExistsIxSet = do
  {indicies,set} <- genIxSet
  a <- arbitrary
  let {index,set: set'} = IxSet.insert a set
  pure $ case IxSet.lookup index set' of
    Nothing -> Failed "No index in map"
    Just value
      | value == a -> Success
      | otherwise -> Failed $ "Value or key doesn't match - original: " <> show a <> ", found: " <> show value


deleteDoesntExistIxSet :: Gen Result
deleteDoesntExistIxSet = do
  {indicies,set} <- genIxSet
  a <- arbitrary
  let {index, set: set'} = IxSet.insert a set
      set'' = IxSet.delete index set'
  pure $ case IxSet.lookup index set'' of
    Nothing -> Success
    Just value -> Failed $ "Found value when shouldn't exist - original: " <> show a <> ", found: " <> show value


jsonIsoIxSet :: Gen Result
jsonIsoIxSet = do
  {indicies,set} <- genIxSet
  pure $ case decodeJson (encodeJson set) of
    Left e -> Failed $ "Json decoding failed: " <> e
    Right set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'

abIsoIxSet :: Gen Result
abIsoIxSet = do
  {indicies,set} <- genIxSet'
  let ab = unsafePerformEffect (encodeArrayBuffer set)
      mSet' = unsafePerformEffect (decodeArrayBuffer ab)
  pure $ case mSet' of
    Nothing -> Failed "ArrayBuffer decoding failed"
    Just set'
      | set' == set -> Success
      | otherwise -> Failed $ "Sets not equal - original: " <> show set <> ", parsed: " <> show set'


genIxSet :: Gen {set :: IxSet Int, indicies :: Array Index}
genIxSet = do
  (xs :: Array _) <- arbitrary
  let go x {indicies,set: set'} =
        let {index,set} = IxSet.insert x set'
        in  { indicies: Array.snoc indicies index
            , set
            }
  pure $ foldr go {set: IxSet.empty, indicies: []} xs

genIxSet' :: Gen {set :: IxSet Int32BE, indicies :: Array Index}
genIxSet' = do
  xs <- arrayOf (Int32BE <$> arbitrary)
  let go x {indicies,set: set'} =
        let {index,set} = IxSet.insert x set'
        in  { indicies: Array.snoc indicies index
            , set
            }
  pure $ foldr go {set: IxSet.empty, indicies: []} xs

