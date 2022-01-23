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
