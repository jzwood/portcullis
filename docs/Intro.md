# Intro to Portcullis

Many languages claim to be simple but few truly are. While Portcullis's compiler
is arguably simple, clocking in around 700 LOC, writing Portcullis programs can
be a bit of a puzzle. This is mostly the result of its restricted and minimalist
syntax and design decisions:

- no looping constructs (use recursion)
- no variable declaration
- no built-in `Booleans` (use `Atoms`)
- no `null` or `error` types
- no side-effects (use pipes)

A compiled Portcullis module cannot be run (on its own) -- rather the Portcullis
compiler produces a JavaScript file with exported javascript functions and a
pipeline graph data-structure.
