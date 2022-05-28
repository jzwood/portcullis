## TODO

- write typechecking unit tests for tuples
- there's a very interesting bug with Guards. if you have some type binding in the predicate that binding is not honored in later expressions.

e.g.
```
hofBad -> -> a Atom -> [a] [a]
hofBad f xs =
  ?  (f xs) xs
  ?? xs
```

so `(f xs)` sets `a` to `[Num]` if arg 2 is a num list. But the signature at the end doesn't realize it should now be `[[Num]]`.


## TODO
- assess above todos
- update unit tests
- update vim theme for latest syntax (done??)

## TODO
- fix hof typechecking bug  (done??)
- figure out some standard lib or module system or something -- it's annoying having to copy in code for
    - length, concat, tail, not, and, drop, take, slice, map,
- update documentation for cons-uncons
- add more code examples in readme
  - update Examples.md

## BUG ISSUE (?)
okay, so I think I know the issue. when HOF expressions are are typechecked after value expressions the final signature can have the wrong variable.

```
id -> z z
id x = x

hm1 -> w -> -> w w w
hm1 z f = (f z)

test -> q q
test x = (hm1 x id)
```

in a world where value expressions are evaluated last it seems to do the correct binding (ie changing order or params in hm1).

## BUG SOLUTION (?)
so the compiler can potentially typecheck expressions in ideal order regardless of the actual order of params in function statement (?)
