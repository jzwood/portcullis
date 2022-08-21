## TODO

- write typechecking unit tests for tuples
- update unit tests
- update vim theme for latest syntax (done but not pushed)
- fix hof typechecking bug  (done??)
- update documentation for cons-uncons




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


Hmm. ok. screw it. I think I'm gonna just make typecheck lazier and if the final types are structurall the same it type checks. aka [a] == [e] will be true. I _think_ we just let whatever function eventually binds it to a concrete type fail the typecheck :shrug:

## Make List easier???
it's annoying you always have to specify the type of the list, wouldn't it be better to do `[1 2 3]` instead of `Num [1 2 3]`? Also, wouldn't `"JAKE"` be better than `Char ['J', 'A', 'K', 'E]`? dunno, actually. maybe the pain is good / the complexity is not worth it.

<|> List <$> trimLeft (CharType <$ word (show CharType)) <*> trimLeft (brack $ trim $ zeroOrMore parseExpr)  -- charlist sugar: "HELLO"

