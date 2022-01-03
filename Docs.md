# Portcullis Documentation

Many languages claim to be simple but few truly are. Portcullis's simplicity borders on annoying. The entirety of the language consists of function declaration, function invocation, and a handful of datatypes and built-in operators.

### Data Types

| Data Type | Example | Operators |
| --- | --- | --- |
| Number | `2.3` | `+` `-` `*` `/` `%` `>` `<` `>=` `<=` `==` |
| Character | `'c'` | `==` |
| Atom | `One`, `True` | `==` |
| 2-Tuple | `['A' True]` | `@1` `@2` `==` |
| Array | `Num [1 2 3 4]`, `Char ['j' 'a' 'k' 'e']` | `!` `!!` |

### Function Declaration
syntax:
```
<function name> <signature>
<function name> [args...] = <expression>
```
example:

```
double -> Num Num
double x = * 2 x
```

### Control Flow:
Guards are the only mechanism for controling program flow
```
?  <predicate> <expression>
?  <predicate> <expression>
?? <expression>
```

Guards expect 1 or more cases of a predicate followed by an expression. A predicate, in this context, is an expression that resolves to an `Atom`. The first atom that is not `False` is the path that is picked. If all case predicates resolve to `False` then default case is used (ie the `??` case).

### Function Declaration
All functions are composed of a signature and a function declaration which includes parameters and a body expression.

## Syntax
- unary operators
- binary operators
- ternary operators
