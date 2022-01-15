# Portcullis

A *simple* programming language


**source fibonacci.po**

```
fib -> Num Num
fib n =
  ?  <= n 1 1
  ?? + (fib - n 1) (fib - n 2)
```

**compiled fibonacci.js**

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
- [Language Guide](Docs.md)
- [User Guide](UserGuide.md)
- [Developer Guide](LocalDev.md)
