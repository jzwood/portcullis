#!/usr/bin/env lua

require "test.codegen.lua.compiled.hof"
require "test.codegen.lua.unittest"

assertEqual("double", double(4), 8)
assertEqual("id1", id1(2), 2)
assertEqual("id2", id2(3), 3)
assertEqual("one1", one1, 1)
assertEqual("one2", one2, 1)
assertEqual("quadruple", quadruple(4.5), 18)

print("PASSED", "hof.test.lua")
