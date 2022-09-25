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

# Big Idea

> Functions don't control data-flow

Put another way, functions only manipulate data. Pipes and Addresses describe how functions asyncronously compose.

Portcullis lets you define
let's you create functions, static named typed streams, and describe how streams compose via functions to produce an execution. The compiler doesn't spit out a runtime but it typechecks the execution graph and exports a datastructure that describes the graph. It is then up to users/lib creators to implement a concurrent runtime that consumes this datastructure and execute the dataflow. That means that you could have one runtime that uses DOM events to propagate event, or one that uses broadcasts channels, or one that instantiates web workers and uses worker message passing so it's both concurrent and in parallelized. The runtime could choose to implement a telemetry dashboard where the whole system is observable in realtime. Since you've got a datastructure that describes all dataflow, systems diagrams can be programmatically generated instead of done by hand as documentation. I have a proof of concept that does just that.

# Philosophy
