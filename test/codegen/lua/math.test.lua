#!/usr/bin/env lua

require "test.codegen.lua.compiled.math"
require "test.codegen.lua.unittest"

assertEqual("avg", avg(6)(10), 8)
assertEqual("compose", compose(_plus_(1))(_mult_(2))(3), 7)
assertEqual("mean", mean({head = 5, tail = { head = 10, tail = { head = 15, tail = {}}}}), 10)
local list = { head = 1, tail = { head = 4 , tail = { head = 2, tail = {}}}}
local sortedList = { head = 1, tail = { head = 2 , tail = { head = 4, tail = {}}}}
assertEqual("msort", msort(list), sortedList)
assertEqual("qsort", qsort(list), sortedList)
assertEqual("neg", neg(7), -7)
assertEqual("total", total({ head = 8, tail = { head = 4 , tail = { head = 7, tail = {}}}}), 19)
assertEqual("tail", tail({ head = 1, tail = { head = 2 , tail = { head = 3, tail = {}}}}), { head = 2 , tail = { head = 3, tail = {}}})

print("PASSED", "math.test.lua")
