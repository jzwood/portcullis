import _ from "https://deno.land/x/lodash@4.17.15-es/lodash.js";
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.119.0/testing/asserts.ts";
import {
  compose,
  double,
  id,
  id2,
  one1,
  one2,
  quadruple,
} from "./compiled/hof.js";

const generateNumArray = () =>
  Array(Math.floor(100 * Math.random()))
    .fill(0)
    .map(() => Math.random() * 10 * (Math.random() < 0.5 ? 1 : -1));
const nativeSort = (arr) => arr.sort((a, b) => a - b);
const epsilon = 0.00001;
const fuzzyEqual = (a, b) => Math.abs(b - a) < epsilon;

Deno.test("double", () => {
  const nums = generateNumArray();
  nums.forEach((n) => {
    assertEquals(double(n), 2 * n);
  });
});

Deno.test("quadruple", () => {
  const nums = generateNumArray();
  nums.forEach((n) => {
    assertEquals(quadruple(n), 4 * n);
  });
});

Deno.test("id/2", () => {
  const nums = generateNumArray();
  nums.forEach((n) => {
    assertEquals(id(n), n);
    assertEquals(id2(n), n);
  });
});

Deno.test("one", () => {
  assertEquals(1, one1);
  assertEquals(1, one2);
});
