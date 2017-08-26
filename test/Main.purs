module Test.Main where

import Prelude

import Control.Monad.Eff.Console (log)
import Data.Foldable (intercalate)
import Data.Nullable (Nullable)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (writeTextFile)
import OhYes (FakeSumRecord(..), generateTS)
import Text.Prettier (defaultOptions, format)
import Type.Proxy (Proxy(..))
import Type.Row (Cons)

type A =
  { a :: Number
  , b :: String
  , c :: { d :: String }
  , e :: Array String
  , f :: Nullable String
  }

type FakeSumRecordTest = FakeSumRecord
  ( a :: String
  , b :: Number
  , c :: Boolean
  )

main :: _
main = do
  writeTextFile UTF8 "./test/generated.ts" contents
  log "types generated"
  where
    contents = format defaultOptions $ intercalate "\n"
      [ generateTS "A" (Proxy :: Proxy A)
      , generateTS "FakeSumRecordTest" (Proxy :: Proxy FakeSumRecordTest)
      ]
