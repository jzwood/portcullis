require "test.codegen.lua.compiled.math"

function assertEqual(fxnName, a, b)
    print(fxnName, a, b)
    assert(_eq_(a)(b) == 1, fxnName)
end
