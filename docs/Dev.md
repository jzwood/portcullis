# Portcullis Developer Guide

### Compile Portcullis Program

- install stack
- build compiler
  - `stack install`
- write Portcullis program
  ```
  neg -> Num Num
  neg x = - 0 x
  ```
- compile
  - `portcullis-exe path/to/src.po path/to/dest.js`
- see output
  ```
  // function "neg" has type (Num -> Num)
  export function neg(x) {
    return (0.0 - x);
  }
  ```

### Testing

**run all tests**

- `bash test.sh --all`

_**note:** make sure you have deno, python3, and lua installed._

**codegen tests**

- `bash test.sh`

**doctests**

- `stack test :doctests`

**hspec tests**

- `stack test`

**Deno Tests**

- install deno
- run tests
  - `deno test`
  - `python3 test/codegen/py/hof.test.py && python3 test/codegen/py/math.test.py`
  - `lua test/codegen/lua/hof.test.lua && lua test/codegen/lua/math.test.lua`

- add test example
  - write po file in `test/codegen/<name>.po`
  - update `test/codegen/CompileTests.hs` to include new program
  - run compilation `stack runhaskell test/codegen/CompileTests`
    - `test/codegen/js/compiled/<name>.js` should now exist
  - write js test file, `test/codegen/<name>.test.js`, that imports functions
    from `test/codegen/js/compiled/<name>.js`
  - you can now call `deno test`

### REPL

#### Deno

[Deno](https://deno.land/manual@v1.22.0) `v1.22.0` or higher is the recommended
backend runtime for compiled portcullis programs.

    deno repl --unstable --eval-file=<path/to/file.js>

&ast; _At the moment the `BroadcastChannel` API requires the `--unstable` flag
to be exposed._

#### Python

    python3 <path/to/file.py>

#### Lua

    lua -u <path/to/file.lua>
