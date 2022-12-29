# Intro to Portcullis

Many languages claim to be simple but few truly are. While Portcullis's compiler
is arguably simple, clocking in well under 1000 LOC, writing Portcullis programs can
be a bit of a puzzle. This is mostly the result of its restricted syntax and
minimalist design which include:

- no looping constructs (use recursion)
- no variable declaration
- no built-in `Booleans` (use `Atoms`)
- no `null` type or `error` primitives such as `Exception`
- no side-effects (use pipes)

Despite these limitations, one can still build useful software that is

- concurrent
- free from runtime exceptions*
- highly observable

# Big Idea

> Functions don't control data-flow

Put another way, functions only manipulate data. Pipes and Streams describe how
functions asyncronously compose.

Portcullis lets you define functions, streams, and pipes. Functions manipulate
date, streams both namespace data and add type signature, and pipes describe how
streams and functions compose.

Even though the Portcullis compiler is runtime agnostic it still typechecks the
execution graph it exports. It is up to users/lib creators to implement a
concurrent runtime that executes this exported dataflow graph. That means that
you could have one runtime that uses DOM events to propagate event, or one that
uses broadcasts channels, or one that instantiates web workers and uses worker
message passing so it's both concurrent and parallelized. The runtime could
choose to implement a telemetry dashboard where the whole system is observable
in realtime. Since you've got a datastructure that describes all dataflow,
systems diagrams can be programmatically generated instead of done by hand.

Look at the demos in _/runtime/examples_ to see examples.

### footnotes

\* barring a compiler bug
