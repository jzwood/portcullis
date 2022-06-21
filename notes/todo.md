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
- write negative tests
    - i.e. write a bunch of functions that parse but do not (and should not) succeed typecheck. have tests confirm this


### BUGFIX SOLUTION IDEA
So typecheckFunc calls `typeofBody <- typeofExpr funcMap func body` to get the resultant type of an expression. It then compares with the signature and flags an error if they don't match. I think that if typeofExpr returned the unspecified type map we'd have enough information to know if two unspecified types with different names are, in fact, equivalent.

e.g.

Unspecified a /= Unspecified b
but if "a" -> Unspecified b then they are equal
