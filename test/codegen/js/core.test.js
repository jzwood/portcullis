import _ from "https://deno.land/x/lodash@4.17.15-es/lodash.js";
import {
  assert,
  assertEquals,
} from "https://deno.land/std@0.119.0/testing/asserts.ts";
import { _equal as equal } from "../../../src/core.js";

Deno.test("equals", () => {
  const uniqThings = [
    [],
    [[10, -23], [1, 2, 3], [3, 5], []],
    [[], []],
    [[[[[[[[]]]]]]]],
    [[[["j"]], [["a"], [["k"]]]]],
  ];
  uniqThings.forEach((x, i) => {
    uniqThings.forEach((y, j) => {
      if (i === j) {
        assert(equal(x, y));
      } else {
        assert(!equal(x, y));
      }
    });
  });
});
