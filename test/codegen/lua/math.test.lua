#!/usr/bin/env lua

require "compiled.math"
require "unittest"

--assertEqual("avg", avg(6)(10), 8)
--assertEqual("compose", compose(_plus_(1))(_mult_(2))(3), 7)
--assertEqual("mean", mean({head = 5, tail = { head = 10, tail = { head = 15, tail = {}}}}), 10)

--assertEqual(
    --"msort",
    --msort({ head = 1, tail = { head = 4 , tail = { head = 2, tail = {}}}}),
    --{ head = 1, tail = { head = 2 , tail = { head = 4, tail = {}}}}
    --)


printList(qsort({ head = 1, tail = { head = 4 , tail = { head = 2, tail = {}}}}))
print("X")
printList({ head = 1, tail = { head = 2 , tail = { head = 4, tail = {}}}})

assertEqual(
    "qsort",
    qsort({ head = 1, tail = { head = 4 , tail = { head = 2, tail = {}}}}),
    { head = 1, tail = { head = 2 , tail = { head = 4, tail = {}}}}
    )

--assertEqual("neg", neg(7), -7)
--assertEqual("total", total({ head = 8, tail = { head = 4 , tail = { head = 7, tail = {}}}}), 19)
--assertEqual(
    --"tail",
    --tail({ head = 1, tail = { head = 2 , tail = { head = 3, tail = {}}}}),
    --{ head = 2 , tail = { head = 3, tail = {}}}
    --)

print("PASSED", "math.test.lua")




