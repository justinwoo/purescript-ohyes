module OhYes where

import Prelude

import Data.Foldable (intercalate)
import Data.Foreign (Foreign)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.Nullable (Nullable)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

toTS :: forall a. HasTSRep a => a -> a
toTS = id

generateTS :: forall a
   . HasTSRep a
  => String -> Proxy a -> String
generateTS name _ = "export type " <> name <> " = " <> ty
  where
    p = Proxy :: Proxy a
    ty = toTSRep p

class HasTSRep a where
  toTSRep :: Proxy a -> String

instance numberHasTSRep :: HasTSRep Number where
  toTSRep _ = "number"

instance stringHasTSRep :: HasTSRep String where
  toTSRep _ = "string"

instance booleanHasTSRep :: HasTSRep Boolean where
  toTSRep _ = "boolean"

instance nullableHasTSRep ::
  ( HasTSRep a
  ) => HasTSRep (Nullable a) where
  toTSRep _ = ty <> " | null"
    where
      p = Proxy :: Proxy a
      ty = toTSRep p

instance arrayHasTSRep ::
  ( HasTSRep a
  ) => HasTSRep (Array a) where
  toTSRep _ = toTSRep p <> "[]"
    where
      p = Proxy :: Proxy a

instance recordHasTSRep ::
  ( RowToList row rl
  , HasTSRepFields rl
  ) => HasTSRep (Record row) where
  toTSRep _ = "{" <> fields <> "}"
    where
      rlp = RLProxy :: RLProxy rl
      fields = intercalate "," $ toTSRepFields rlp

class HasTSRepFields (rl :: RowList) where
  toTSRepFields :: RLProxy rl -> List String

instance consHasTSRepFields ::
  ( HasTSRepFields tail
  , IsSymbol name
  , HasTSRep ty
  ) => HasTSRepFields (Cons name ty tail) where
  toTSRepFields _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      val = toTSRep typ
      head = key <> ":" <> val
      tailp = RLProxy :: RLProxy tail
      tail = toTSRepFields tailp

instance nilHasTSRepFields :: HasTSRepFields Nil where
  toTSRepFields _ = mempty


newtype FakeSumRecord (magicPairings :: # Type) = FakeSumRecord
  { tag :: String
  , content :: Foreign
  }

instance fakeSumRecordHasTSRep ::
  ( RowToList row rl
  , FakeSumRecordMembers rl
  ) => HasTSRep (FakeSumRecord row) where
  toTSRep _ = intercalate "|" members
    where
      rlp = RLProxy :: RLProxy rl
      members = toFakeSumRecordMembers rlp

class FakeSumRecordMembers (rl :: RowList) where
  toFakeSumRecordMembers :: RLProxy rl -> List String

instance consFakeSumRecordMembers ::
  ( FakeSumRecordMembers tail
  , IsSymbol name
  , HasTSRep ty
  ) => FakeSumRecordMembers (Cons name ty tail) where
  toFakeSumRecordMembers _ = head : tail
    where
      namep = SProxy :: SProxy name
      key = reflectSymbol namep
      typ = Proxy :: Proxy ty
      val = toTSRep typ
      head = "{tag:\"" <> key <> "\", content:" <> val <> "}"
      tailp = RLProxy :: RLProxy tail
      tail = toFakeSumRecordMembers tailp

instance nilFakeSumRecordMembers :: FakeSumRecordMembers Nil where
  toFakeSumRecordMembers _ = mempty

-- type Fruit | {type: "Apple" } | {type: "Banana", content: { color: string } }

-- type: "Apple"
-- type: "Banana", content: {color: string}

-- class MagicPairing (name :: Symbol) ty
-- instance asdf :: MagicPairing "Banana" Number

-- instance asdfsdf ::
--   ( IsSymbol name
--   , MagicPairing name ty
--   ) => MyMagicThing (Cons name ty tail)
