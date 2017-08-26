import * as Gen from "./generated";

const a1: Gen.A = {
  a: 123,
  b: "asdf",
  c: {
    d: "asdf"
  },
  e: ["asdf"],
  f: "asdf"
};

const a2: Gen.A = {
  a: 123,
  b: "asdf",
  c: {
    d: "asdf"
  },
  e: ["asdf"],
  f: null
};

const fakeUnion1: Gen.FakeSumRecordTest = {
  tag: "a",
  content: "asdf"
};

const fakeUnion2: Gen.FakeSumRecordTest = {
  tag: "b",
  content: 123
};

const fakeUnion3: Gen.FakeSumRecordTest = {
  tag: "c",
  content: true
};
