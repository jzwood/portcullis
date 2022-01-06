import { assert, assertEquals } from "https://deno.land/std@0.119.0/testing/asserts.ts";
import { avg } from './math.js'

const average = (a, b) => (a + b) / 2

Deno.test("avg", () => {
  const pairs = [
    [10, -23],
    [90, 23.34],
    [-2, -24.542]
  ]
  pairs.forEach(([x, y]) => {
    assertEquals(avg(x)(y), average(x, y))
  })
})
