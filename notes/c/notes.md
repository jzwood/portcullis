 where I return a function pointer plus the argument to be applied? something like

```
function add(a) {
 return (a, _add)
}
function add_1(b) {
 return (b, add_2)
}
function add_2(c) {
 return c
}
```
