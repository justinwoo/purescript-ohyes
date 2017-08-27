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
