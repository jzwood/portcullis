# portcullis

A *simple* programming language


**src/fibonacci.po**

```
fib -> Num Num
fib n =
  ?  <= n 1 1
  ?? + (fib - n 1) (fib - n 2)
```

**dest/fibonacci.js**

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

### more
- [portcullis developer setup](LocalDev.md)
