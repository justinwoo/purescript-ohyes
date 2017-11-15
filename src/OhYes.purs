module OhYes where

import Prelude

import Data.Foldable (intercalate)

import Data.Function.Uncurried (Fn2)
import Data.List (List, (:))
import Data.Monoid (mempty)
import Data.Nullable (Nullable)

import Data.Variant (Variant)
import Type.Prelude (class IsSymbol, SProxy(..), reflectSymbol)
import Type.Proxy (Proxy(..))
import Type.Row (class RowToList, Cons, Nil, RLProxy(..), kind RowList)

-- | identity function for applying the HasTSRep constraint
toTS :: forall a. HasTSRep a => a -> a
toTS = id

-- | Generate Typescript type signatures for a given type, supplying a name to use as the type name
generateTS :: forall a
   . HasTSRep a
  => String -> Proxy a -> String
generateTS name _ = "export type " <> name <> " = " <> ty
  where
    p = Proxy :: Proxy a
    ty = toTSRep p

-- | Our main type class for types that can be represented in Typescript types without conversion. You may want to using newtype instance deriving for this class for your newtypes, but any other types should be tested for correctness.
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

instance functionHasTSRep ::
  ( HasTSRep a
  , HasTSRep b
  ) => HasTSRep (Function a b) where
  toTSRep _ = "(a: " <> a <> ") => " <> b
    where
      a = toTSRep (Proxy :: Proxy a)
      b = toTSRep (Proxy :: Proxy b)

instance fn2HasTSRep ::
  ( HasTSRep a
  , HasTSRep b
  , HasTSRep c
  ) => HasTSRep (Fn2 a b c) where
  toTSRep _ =
      "(a: " <> a <>
      ", b: " <> b <>
      ") => " <> c
    where
      a = toTSRep (Proxy :: Proxy a)
      b = toTSRep (Proxy :: Proxy b)
      c = toTSRep (Proxy :: Proxy c)

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

-- | a Variant is represented by VariantRep, which is a newtype record of
-- | `newtype VariantRep a = VariantRep { type ∷ String , value ∷ a }`
-- | as seen here:
-- | https://github.com/natefaubion/purescript-variant/blob/aef507e2972d294ecd735575371eccbc61ac1ac4/src/Data/Variant/Internal.purs#L31
instance fakeSumRecordHasTSRep ::
  ( RowToList row rl
  , FakeSumRecordMembers rl
  ) => HasTSRep (Variant row) where
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
      head = "{type:\"" <> key <> "\", value:" <> val <> "}"
      tailp = RLProxy :: RLProxy tail
      tail = toFakeSumRecordMembers tailp

instance nilFakeSumRecordMembers :: FakeSumRecordMembers Nil where
  toFakeSumRecordMembers _ = mempty
