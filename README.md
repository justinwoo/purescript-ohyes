# Purescript-OhYes

[![Build Status](https://travis-ci.org/justinwoo/purescript-ohyes.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-ohyes)

A library for generating Typescript types that can be used transparently from Purescript.

![](http://i.imgur.com/ZlX0iGz.png)

This library also provides ways for working with typical union type forms of records with a discriminant field by providing a `VariantRecord` structure that generates types in the following form:

```ts
export type VariantRecordTest =
  | { tag: "a", content: string }
  | { tag: "b", content: number }
  | { tag: "c", content: boolean };
```

## Example

The [tests](test/Main.purs) generate types and write them to a file, which is then checked with Typescript using [test/test.ts](test/test.ts):

```hs
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

generateTSFile :: _
generateTSFile = writeTextFile UTF8 "./test/generated.ts" contents
  where
    contents = format defaultOptions $ intercalate "\n"
      [ generateTS "A" (Proxy :: Proxy A)
      , generateTS "VariantRecordTest" (Proxy :: Proxy VariantRecordTest)
      ]
```

Generated types:

```ts
export type A = {
  a: number,
  b: string,
  c: { d: string },
  e: string[],
  f: string | null,
  g: (a: number) => (a: number) => number,
  h: (a: number, b: number) => number,
  i: (a: number, b: (a: number, b: number) => number) => number
};
export type VariantRecordTest =
  | { tag: "a", content: string }
  | { tag: "b", content: number }
  | { tag: "c", content: boolean };
```
