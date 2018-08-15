module Test.Main where

import Prelude

import Data.Foldable (intercalate)
import Data.Function.Uncurried (Fn2)
import Data.Nullable (Nullable)
import Data.Variant (Variant)
import Effect (Effect)
import Effect.Class (liftEffect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import OhYes (generateTS)
import Test.Spec (describe, it)
import Test.Spec.Reporter (consoleReporter)
import Test.Spec.Runner (run)
import Text.Prettier (defaultOptions, format)
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

type VariantTest = Variant
  ( a :: String
  , b :: Number
  , c :: Boolean
  )

main :: Effect Unit
main = run [consoleReporter] do
  describe "purescript-ohyes" do
    describe "codegen" do
      it "can generate types" do
        liftEffect generateTSFile

generateTSFile :: Effect Unit
generateTSFile = writeTextFile UTF8 "./test/generated.ts" values
  where
    values = format defaultOptions $ intercalate "\n"
      [ generateTS "A" (Proxy :: Proxy A)
      , generateTS "VariantTest" (Proxy :: Proxy VariantTest)
      ]

