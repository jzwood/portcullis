//import _ from 'https://deno.land/x/lodash@4.17.15-es/lodash.js';
import { assert, assertEquals } from "https://deno.land/std@0.119.0/testing/asserts.ts";

import { sort } from './rsort.js'

const generateNumArray= () => Array(Math.floor(100 * Math.random())).fill(0).map(() => Math.random() * 10 * (Math.random() < 0.5 ? 1 : -1))

const nativeSort = (arr) => arr.sort((a, b) => a - b)

Deno.test("sorting", () => {
  for (let i=0; i<20; i++) {
    const arr = generateNumArray();
    assertEquals(sort(arr), nativeSort(arr));
  }
});
