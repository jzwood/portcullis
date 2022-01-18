<p align="center">
	<img src="./assets/logo.svg" width="150"/>
</p>

<p align="center">
	<h1 align="center">Portcullis</h1>
  <h4 align="center">A minimalist programming language.</h4>
</p>


fib.po

<img src="./assets/fib.po.png" alt="portcullis source code for Fibonacci function" title="fib -> Num Num
fib n =
  ?  <= n 1 1
  ?? + (fib - n 1) (fib - n 2)">

compiles to fib.js

```js
// function "fib" has type (Num -> Num)
function fib(n) {
	return (() => {
		if ((n<=1.0)) {
			return 1.0;
		}
		return (fib((n-1.0))+fib((n-2.0)));
	})();
}
```

## About

I wrote the compiler for Portcullis purely for my own education and amusement. Painfully specific goals guided the development of this project and account for both the language features I chose to include as well as omit.

## # More
- [Language Guide](./docs/Docs.md)
- [Developer Guide](./docs/LocalDev.md)
- [Vim Syntax Highlighting](https://github.com/jzwood/portcullis-vim)
