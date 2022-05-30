<p align="center">
	<img src="./assets/logo.svg" width="150"/>
</p>

<p align="center">
	<h1 align="center">Portcullis</h1>
  <h4 align="center">A minimalist programming language.</h4>
</p>


fib.po

<img
  src="./assets/fib.po.png"
  alt="portcullis source code for Fibonacci function"
  title="portcullis implementation of n fibonacci number"
  style="width: 100%; max-width: 500px;"
/>

compiles to fib.js

```js
// function "fib" has type (Num -> Num)
export function fib(n) {
  return (
    /* if */ (n <= 1.0) ?
    /* then */ 1.0 :
    /* else */ (fib((n - 1.0)) + fib((n - 2.0)))
  );
}
```

<hr>

fizzbuzz.po

<img
  src="./assets/fizzbuzz.po.png"
  alt="portcullis source code for Fizzbuzz algorithm"
  title="portcullis source code for Fizzbuzz algorithm"
  style="width: 100%; max-width: 500px;"
/>

```
Deno 1.22.0
exit using ctrl+d or close()
> fizzbuzz(20)
[
   1,  2, -1,  4, -2, -1,  7,
   8, -1, -2, 11, -1, 13, 14,
  -3, 16, 17, -1, 19, -2
]
```

## About

I wrote the compiler for Portcullis purely for my own education and amusement. Painfully specific goals guided the development of this project and account for both the language features I chose to include as well as omit.

Some of these goals include
- minimalism
- [purity](https://en.wikipedia.org/wiki/Purely_functional_programming)
- [parametric polymorphism](https://en.wikipedia.org/wiki/Parametric_polymorphism)

## More
- [Language Guide](./docs/Docs.md)
- [Developer Guide](./docs/Dev.md)
- [Vim Syntax Highlighting](https://github.com/jzwood/portcullis-vim)
