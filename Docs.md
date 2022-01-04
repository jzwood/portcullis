# Portcullis Language Guide

Many languages claim to be simple but few truly are. Portcullis's simplicity borders on annoying. The entirety of the language consists of function declaration, function invocation, and a handful of datatypes and built-in operators.

### Data Types

| Data Type | Example | Operators* |
| --- | --- | --- | --- |
| Number | `2.3` | `+` `-` `*` `/` `%` `>` `<` `>=` `<=` `==` |
| Character | `'c'` | `==` |
| Atom | `One`, `True` | `==` |
| 2-Tuple | `['A' True]` | `@1` `@2` `==` |
| Array | `Num [1 2 3 4]`, `Char ['j' 'a' 'k' 'e']` | `!` `!!` |

_* All operators are prefix (ie not infix)._

### Types

| Type | Syntax |
| --- | --- | --- |
| Number | `Num` |
| Character | `Char` |
| Atom | `Atom` |
| Unspecified | `a`, `x` |
| 2-Tuple | `[<type> <type>]` |
| Array | `[<type>]` |
| Arrow | `-> <type <type>>` |

### Functions

```
double -> Num Num
double x = * 2 x
```

```
compose -> -> a b -> a b
compose f g x = (f (g x))
```
Compiling the above portcullis functions results in exportable/runnable JS functions:

```
compose(double, double, 3)
// 12
```

Functions are comprised of a **name**, **type signature**, **arguments**, and body **expression**:
```
<name <type>>
<name> [args...] = <expr>
```

_Portcullis does not have lambda functions._

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
