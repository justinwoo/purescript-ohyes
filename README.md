# Purescript-OhYes

[![Build Status](https://travis-ci.org/justinwoo/purescript-ohyes.svg?branch=master)](https://travis-ci.org/justinwoo/purescript-ohyes)

A library for generating Typescript types that can be used transparently from Purescript.

See the blog post here: <https://github.com/justinwoo/my-blog-posts#you-can-interop-with-typescript-using-purescript-ohyes>

This is likely not the "silver bullet" that you are looking for, but can give you some good ideas on how to get going.

![](http://i.imgur.com/ZlX0iGz.png)

This library also provides ways for working with typical union type forms of records with a discriminant field by using [Variant](https://github.com/natefaubion/purescript-variant), which uses a record representation with a string literal `type` field and the associated `value` field.

```ts
export type VariantTest =
  | { type: "a", value: string }
  | { type: "b", value: number }
  | { type: "c", value: boolean };
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

type VariantTest = Variant
  ( a :: String
  , b :: Number
  , c :: Boolean
  )

generateTSFile :: _
generateTSFile = writeTextFile UTF8 "./test/generated.ts" values
  where
    values = format defaultOptions $ intercalate "\n"
      [ generateTS "A" (Proxy :: Proxy A)
      , generateTS "VariantTest" (Proxy :: Proxy VariantTest)
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
export type VariantTest =
  | { type: "a", value: string }
  | { type: "b", value: number }
  | { type: "c", value: boolean };
```

See the additional example here: https://github.com/justinwoo/ohyes-demo
