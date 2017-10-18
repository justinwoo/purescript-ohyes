module Test.Main where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Data.Foldable (intercalate)
import Data.Foreign (Foreign, toForeign, unsafeFromForeign)
import Data.Function.Uncurried (Fn2)
import Data.Nullable (Nullable)
import Data.Variant (default, inj, on)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import OhYes (VariantRecord(..), generateTS, overVariant)
import Test.Spec (describe, it)
import Test.Spec.Assertions (shouldEqual)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Text.Prettier (defaultOptions, format)
import Type.Prelude (SProxy(..))
import Type.Proxy (Proxy(..))

type A =
  { a :: Number
  , b :: String
  , c :: { d :: String }
  , e :: Array String
  , f :: Nullable String
  , g :: Number -> Number -> Number
  , h :: Fn2 Number Number Number
  , i :: Fn2 Number (Fn2 Number Number Number) Number
  }

type VariantRecordTest = VariantRecord
  ( a :: String
  , b :: Number
  , c :: Boolean
  )

main :: _
main = run [consoleReporter] do
  describe "purescript-ohyes" do
    describe "codegen" do
      it "can generate types" do
        liftEff generateTSFile
    describe "VariantRecord" do
      it "correctly roundtrips to variant" do
        let vr = VariantRecord {tag: "a", content: toForeign "asdf"} :: VariantRecordTest
        let result = overVariant f vr
        eqVariants vr result
          where
            a = SProxy :: SProxy "a"
            f v = default v
              # on a (const $ inj a "qwer")
              $ v
            eqVariants (VariantRecord vrA) (VariantRecord vrB) = do
              vrA.tag `shouldEqual` vrB.tag
              unsafeReadString vrA.content `shouldEqual` "asdf"
              unsafeReadString vrB.content `shouldEqual` "qwer"
            unsafeReadString :: Foreign -> String
            unsafeReadString = unsafeFromForeign

generateTSFile :: _
generateTSFile = writeTextFile UTF8 "./test/generated.ts" contents
  where
    contents = format defaultOptions $ intercalate "\n"
      [ generateTS "A" (Proxy :: Proxy A)
      , generateTS "VariantRecordTest" (Proxy :: Proxy VariantRecordTest)
      ]

