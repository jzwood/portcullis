# Portcullis Language Guide

## Types

| Name        | Grammar                 | Example                            |
| ----------- | ----------------------- | ---------------------------------- |
| Number      | `"Num"`                 | `Num`                              |
| Character   | `"Char"`                | `Char`                             |
| Atom        | `"Atom"`                | `Atom`                             |
| 2-Tuple     | `"{" type type "}"`     | `{Num Char}`                       |
| Array       | `"[" type "]"`          | `[ Atom ]`                         |
| Unspecified | `alphanum { alphanum }` | `a`, `name`                        |
| Arrow       | `"->" type type`        | `-> Num Num`, `-> (-> a b) -> a b` |

## Values

| Name      | Grammer                         | Example                                   |
| --------- | ------------------------------- | ----------------------------------------- |
| Number    | decimal                         | `2.3`                                     |
| Character | char                            | `'c'`                                     |
| Atom      | pascal-word                     | `One`, `True`                             |
| 2-Tuple   | `"{" expression expression "}"` | `{'A' True}`                              |
| Array     | `type ":" "[" { expression } "]"`   | `Num:[1 2 3 4]`, `Char:['j' 'a' 'k' 'e']` |

## Operators

_All operators are prefix (ie not infix)_

| Operator(s)       | Name   | Arity | Type                             | Example                            |
| ----------------- | ------ | ----- | -------------------------------- | ---------------------------------- |
| `@1`              | fst    | 1     | `-> {a b} a`                     | `@1 {'a' 3}`  →  `'a'`             |
| `@2`              | snd    | 1     | `-> {a b} b`                     | `@2 {'a' 3}`  →  `3`               |
| `+` `-` `*` `/`   |        | 2     | `-> Num -> Num Num`              | `+ 3 4`  →  `7`                    |
| `%`               | rem    | 2     | `-> Num -> Num Num`              | `% 7 2`  →  `1`                    |
| `>` `<` `>=` `<=` |        | 2     | `-> Num -> Num Atom`             | `> 1 4`  →  `False`                |
| `==`              |        | 2     | `-> a -> a Atom`                 | `== 7 7`  →  `True`                |
| `+>`              | Cons   | 2     | `-> a -> [a] [a]`                | `+> 1 Num:[2 3]`  →  `Num:[1 2 3]` |
| `<+`              | Uncons | 3     | `-> [a] -> b -> -> a -> [a] b b` | `<+ Num:[1 2 3] 0 sum`  →  `6`     |
| `?`               | If     | 3     | `-> Atom -> a -> a a`            | `? True Cat Dog`  →  `Cat`         |

## Expressions

**grammar**

```
expression = value
           | identifier
           | "(" function-name expression ")"
           | unary-operator expression
           | binary-operator expression expression
           | ternary-operator expression expression expression
```

**examples**

```javascript
3.0
myvar
+ 34 10
(sqrt 5)
```

## Functions

All functions must include a type signature; Portcullis does not have lambda
functions. Additionally, all functions are pure.

**grammar**

```
function = function-name type
           function-name { identifier } "=" expression

function-name = [ "_" ] <alphanum> { <alphanum> }
```

**examples**

```haskell
double -> Num Num
double x = * 2 x

quadruple -> Num Num
quadruple x = (double (double x))
```

## Streams

Streams are static, globally namespaced, and typed data-streams.

**grammar**

```
stream = stream-identifier type
stream-identifier = "&" alphanum { alphanum }
```

**examples**

```haskell
&counter Num
&todo [[Char]]
```

## Pipes

Pipes connect streams via a function. Pipes all taken together can be used to
derive the dataflow diagram for a Portcullis program.

**grammar**

```
pipe = "|" function-name "[" { stream-identifier natural } "]" stream-identifier
```

**examples**

```haskell
| add [ &counter 1 &add 3 ] &counter
```

## Comments

Bracket comments with `#`s. Comments are only allowed between function
statements.

**grammar**

```
comment = "#" { non-hash-char } "#"
```

**example**

```haskell
# comment here #
one Num
one = 1
```

## More Examples

### Currying

```haskell
double -> Num Num
double x = * 2 x

add3 -> Num -> Num -> Num Num
add3 a b c = + + a b c
```

Compiling the above portcullis functions results in exportable/runnable JS
functions:

```javascript
// function "double" has type (Num -> Num)
export function double(x) {
  return (2.0 * x);
}
// function "add3" has type (Num -> (Num -> (Num -> Num)))
export function add3(a) {
  return (b) => (c) => ((a + b) + c);
}
```

```javascript
double(5);
// 10

add3(3)(5)(1);
// 9
```

### Higher-Order Functions

Portcullis

```haskell
_length -> a -> [a] Num
_length x xs = + 1 (length xs)

length -> [a] Num
length xs =
  <+ xs 0 _length
```

JavaScript

```javascript
// function "_length" has type (a -> ([a] -> Num))
export function _length(x) {
  return (xs) => (1.0 + length(xs));
}
// function "length" has type ([a] -> Num)
export function length(xs) {
  return (
    /* if */ equal(xs, [])
      ? /* then */ 0.0
      : /* else */ _length(xs.at(0))(xs.slice(1))
  );
}
```

### Parametric polymorphism

```haskell
_map -> -> q t -> q -> [q] [t]
_map f x xs = +> (f x) (map f xs)

map -> -> t g -> [t] [g]
map f xs =
  <+ xs g:[] (_map f)

add1 -> Num Num
add1 x = + x 1

list [Num]
list = (map add1 Num:[1 2 3 4])
```

```
Deno 1.22.0
exit using ctrl+d or close()
> list
[ 2, 3, 4, 5 ]
```
