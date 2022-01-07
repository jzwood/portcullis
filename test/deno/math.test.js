import _ from 'https://deno.land/x/lodash@4.17.15-es/lodash.js'
import { assert, assertEquals } from "https://deno.land/std@0.119.0/testing/asserts.ts"
import { avg, neg, tail, sort, sum, mean } from './math.js'

const generateNumArray = () => Array(Math.floor(100 * Math.random()))
    .fill(0)
    .map(() => Math.random() * 10 * (Math.random() < 0.5 ? 1 : -1))
const nativeSort = (arr) => arr.sort((a, b) => a - b)
const epsilon = 0.00001
const fuzzyEqual = (a, b) => Math.abs(b - a) < epsilon

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
    0, 45, -23.1, 9
  ]
  nums.forEach((x) => {
    assert(neg(x) === -x)
  })
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
    assertEquals(sort(arr), nativeSort(arr))
  }
  assertEquals(sort([]), [])
})

Deno.test("sum", () => {
  for (let i=0; i<20; i++) {
    const arr = generateNumArray()
    assert(fuzzyEqual(sum(arr), _.sum(arr)))
  }
  assertEquals(sum([]), _.sum([]))
})

Deno.test("mean", () => {
  for (let i=0; i<20; i++) {
    const arr = generateNumArray()
    assert(fuzzyEqual(mean(arr), _.mean(arr)))
  }
})
