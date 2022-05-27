import _ from 'https://deno.land/x/lodash@4.17.15-es/lodash.js'
import { assert, assertEquals } from "https://deno.land/std@0.119.0/testing/asserts.ts"
import { avg, neg, tail, msort, qsort, sum, mean, compose } from './math.js'

const generateNumArray = () => Array(Math.floor(100 * Math.random()))
    .fill(0)
    .map(() => Math.random() * 10 * (Math.random() < 0.5 ? 1 : -1))
const nativeSort = (arr) => arr.sort((a, b) => a - b)
const round = n => parseFloat(n.toFixed(5))

Deno.test("avg", () => {
  const pairs = [
    [10, -23],
    [90, 23.34],
    [-2, -24.542]
  ]
  pairs.forEach(([x, y]) => {
    assertEquals(avg(x)(y), _.mean([x, y]))
  })
})

Deno.test("neg", () => {
  const nums = [
    45, -23.1, 9
  ]
  nums.forEach((x) => {
    assertEquals(neg(x), -x)
  })
  assert(neg(0) === 0)
})

Deno.test("tail", () => {
  for (let i=0; i<20; i++) {
    const arr = generateNumArray()
    assertEquals(tail(arr), arr.slice(1))
  }
  assertEquals(tail([]), [])
})

Deno.test("sort", () => {
  for (let i=0; i<20; i++) {
    const arr = generateNumArray()
    assertEquals(msort(arr), nativeSort(arr))
    assertEquals(qsort(arr), nativeSort(arr))
  }
  assertEquals(msort([]), [])
  assertEquals(qsort([]), [])
})

Deno.test("sum", () => {
  for (let i=0; i<20; i++) {
    const arr = generateNumArray()
    assertEquals(round(sum(arr)), round(_.sum(arr)))
  }
  assertEquals(sum([]), _.sum([]))
})

Deno.test("mean", () => {
  for (let i=0; i<20; i++) {
    const arr = generateNumArray()
    assertEquals(round(mean(arr)), round(_.mean(arr)))
  }
})

Deno.test("compose", () => {
  const add1 = x => x + 1
  const mult2 = x => 2 * x
  const arr = generateNumArray()
  arr.forEach(num => {
    assertEquals(compose(mult2)(add1)(num), 2 * (num + 1))
  })
})
