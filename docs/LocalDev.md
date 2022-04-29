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
    function neg(x) {
        return (0.0-x);
    }
    ```

### Testing

**run all tests**
- `bash test.sh`

**hspec tests**
- `stack test`

**Deno Tests**
- install deno
- run tests
    - `deno test`

- add tests
    - write po file in `test/deno/<name>.po`
    - update `test/deno/CompileTests.hs` to include new program
    - run compilation `stack runhaskell test/deno/CompileTests`
        - `test/deno/<name>.js` should now exist
    - write js test file, `test/deno/<name>.test.js`, that imports functions from `test/deno/<name>.js`
    - you can now call `deno test`
