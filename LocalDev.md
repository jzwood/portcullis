### Transpile Portcullis Program
- install stack
- build compiler
    - `stack install`
- write Portcullis program
    ```
    neg -> Num Num
    neg x = - 0 x
    ```
- run transpiler
    - `portcullis-exe path/to/src.po path/to/dest.js`
- examine output
    ```
    // function "neg" has type (Num -> Num)
    function neg(x) {
        return (0.0-x);
    }
    ```

### Testing
- install deno
- run tests
    - `deno test`
