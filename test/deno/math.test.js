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

//neg -> Num Num
//tail -> [a] [a]
//empty [Num]
//sort -> [Num] [Num]
//sort2 -> Num -> [Num] [Num]
//cmpH -> [Num] -> [Num] [[Num] [Num]]
//merge -> [Num] -> [Num] [Num]
//merge2 -> [[Num] [Num]] [Num]
//avg -> Num -> Num Num
//mean -> [a] Num
//sum -> [Num] Num

