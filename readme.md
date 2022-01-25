# Jq-exercises

This is a learning project, with the following goals:

- Build a compiler for a subset of the jq language
- Use the above to build an app with jq exercises to learn jq
- Learn jq myself as I do the above

## Short term Goals

- find out what I don't know
- define an architecture
- pick a subset of the language to target
- implement compiler + interpreter (is that what it's called) for the subset

## Medium term Goals

- write a jq-exercises web app that uses the compiler/interpreter to help learning jq features
- further extend the supported set of language features

## Long term Goals

- fancy stuff, like
  - syntax highlighting
  - autocomplete
  - helpful error messages (elm-like)
  - module system (probably more than I could chew..)

## Questions

- should I use a Virtual Machine (jq does)? why?
  - I think jq does it for performance reasons, though cannot elaborate how/why?
  - Maybe portability? (though I'm not convinced by this as jq compiles to a binary)
  - Anyway, I don't think I should use a Virtual Machine, mainly to manage the level of complexity
- What is linking?
  - In a language that supports modularisation, linking is necessary to 'join' the various modules after they are independently
  compiled. I am sure there is a ton more nuisance to this, but for now I am happy understanding what a linker is at a high level
- Why does jq use a 'block' representation before generating bytecode?
  - Maybe it's to 'move' one step closer to bytecodewhile retaining some information from the source code that bytecode does
  not need but could be used for things like syntax highlighting
- For parsing should I write one using a parsing library or use something like Bison?
  - Just use a parsing library cause we want to run this in a browser
- Array vs List in Purescript - which to use when / is there a good default?

