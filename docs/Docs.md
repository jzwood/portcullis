# Portcullis Language Guide

Many languages claim to be simple but few truly are. While Portcullis's compiler is arguably simple, clocking in under 700 LOC, writing Portcullis programs can be a bit of a puzzle. This is mostly the result of its restricted and minimalist syntax and uncommon design decisions:

There are no looping constructs (use recursion), no variable declaration, no built-in `Booleans` (use `Atoms`), and no `null` or `error` types.

The entirety of the language consists of function declaration, function invocation, and a handful of datatypes and built-in operators. A compiled Portcullis module cannot be run. It produces JavaScript functions which can be imported and run by other JS programs.


### Data Types

| Name | Type | Example |
| --- | --- | --- |
| Number | `Num` | `2.3` |
| Character | `Char` | `'c'` |
| Atom | `Atom` | `One`, `True` |
| 2-Tuple | `{<type> <type>}` | `{'A' True}` |
| Array | `<type> [<type>]` | `Num [1 2 3 4]`, `Char ['j' 'a' 'k' 'e']` |

## Operators

_All operators are prefix (ie not infix)._

| Operator(s) | Name | Arity | Signature | Example |
| --- | --- | --- | --- | --- |
| `@1` | fst | 1 | `-> {a b} a` | `@1 {'a' 3}`  →  `'a'` |
| `@2` | snd | 1 | `-> {a b} b` | `@2 {'a' 3}`  →  `3` |
| `+` `-` `*` `/` | | 2 |  `-> Num -> Num Num` | `+ 3 4`  →  `7` |
| `%` | rem | 2 | `-> Num -> Num Num` | `% 7 2`  →  `1` |
| `>` `<` `>=` `<=` | | 2 | `-> Num -> Num Atom` | `> 1 4`  →  `False` |
| `==` | | 2 | `-> a -> a Atom` | `== 7 7`  →  `True`|
| `++` | concat | 2 | `-> [a] -> [a] [a]` | `++ [1] [2 3]`  →  `[1 2 3]` |
| `!` | At | 3 | `-> [a] -> Num -> a a` | `! ['a'] 0 'z'`  →  `'a'`, <br> `! ['b'] 3 'z'`  →  `'z'` |
| `!!` | Slice | 3 | `-> [a] -> Num -> Num [a]` | `!! ['j' 'a' 'k' 'e'] 1 3`  →  `['a' 'k']` |
| `?` | If | 3 | `-> Atom -> a -> a a` | `? True Cat Dog`  →  `Cat` |


### Functions

```
double -> Num Num
double x = * 2 x

add3 -> Num -> Num -> Num Num
add3 a b c = + + a b c
```

Compiling the above portcullis functions results in exportable/runnable JS functions:

```
// function "double" has type (Num -> Num)
export function double(x) {
	return (x) => (2*x);
}

// function "add3" has type (Num -> (Num -> (Num -> Num)))
export function add3(a) {
	return (b) => (c) => ((a+b)+c);
}
```

```
double(5)
// 10

add3(3)(5)(1)
// 9
```

Functions are comprised of a **name**, **type signature**, **arguments**, and body **expression**:
```
<name> <type>
<name> [args...] = <expr>
```

Bracket comments with `#`s. Comments are only allowed between function statements.

```
# comment here #
one Num
one = 1
```

_Portcullis does not have lambda functions._
