# Jq-exercises

This is a learning project, with the following high-level goals:

- Build a compiler for a subset of the jq language
- Use the above to build an app with jq exercises to learn jq
- Learn jq, and a bit about compilers as I do the above

## Short term Goals

- find out what I don't know
- define an architecture
- pick a subset of the language to target
- implement compiler + interpreter (is that what it's called?) for the subset

## Medium term Goals

- write a jq-exercises web app that uses the compiler/interpreter to help learning jq features
- further extend the supported set of language features

## Long term Goals

- fancy stuff, like
  - syntax highlighting
  - autocomplete
  - helpful error messages (elm-like)
  - module system (this one is probably more than I can chew..)
    
## Architecture

There are two phases: compiling and interpreting.

The compile phase takes jq source code and turns it into an abstract representation (`Expression`).

The interpreter step takes the Expression, along with input json and returns json.

```
.. compile   ('.foo') -> Select "foo"
String -> Either ParseError AST

.. interpret {"foo": 32} -> (Select "foo") -> 32
JSON -> Expression -> JSON
```

---
## Questions

- What's backtracking in jq and what implications does it have for me? Can I just ignore to start with?
- The documentation describes the language as having _second-class higher-order functions of dynamic extent_ - what does
  this mean?
- Should I use a Virtual Machine (jq does)? why?
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
  - Aside from performance (which I am not going to be concerned about), I think Array should be the default as the literal syntax
    desugars to this type and not List.
- I wonder why the `chainl` combinator asks for a default value and returns that instead of failing?
- Is the pipe operator left associative?
  - I think so, as the evaluation happens left to right, like this: `f(x) | g(f(x)) | h(g(f(x)))`, whereas if it was
    right-associative it would be like ` h(g(f(x))) | f(x) | g(f(x))`
- I still need to wrap my head around how in jq pipes can have multiple outputs. For example, `[1,2,3][]` has 3 separate
  outputs: `1`, `2` adn `3`, which is different from the array we start with (`[1,2,3]`). I am sure there are good reasons
  for this, but I don't know what those are yet.
  Maybe the answer is that this allows iteration, so that if you wanted to map the items in the example above to increment
  by one, then you could do `[ [1,2,3][] | .+1 ]`

##  Things I've learned

### Left recursion
When I started adding support for `Pipe` (`|`), I ran into the left recursion problem, which took me a while to even
realise what the problem was. [This blog post](https://github.com/glebec/left-recursion) describes the issue really well
and helped me climb out of that hole!

The blog post above mentions [this page](https://www.csd.uwo.ca/~mmorenom/CS447/Lectures/Syntax.html/node8.html), which
is a mathematical explanation of how the problem is solved. I'd love to be able to understand the math version.

### Associative Property
I ran into this when looking up the associativity of the `,` and `|` operators. Intuitively, the `|` operator seems to be
left associative (see my thinking in the questions section), however the language specification says it's right associative
(see [here](https://github.com/stedolan/jq/wiki/jq-Language-Description#operators-priority)).

It turns out that the pipe operator will give the same result, regardless of how it's associated. This property is called
associative property and described in [this wiki](https://en.m.wikipedia.org/wiki/Associative_property) like so:

> Within an expression containing two or more occurrences in a row of the same associative operator,
> the order in which the operations are performed does not matter as long as the sequence of the operands is not changed.

Addition is an example of this property, where it doesn't matter how you parenthesise, it will always yield the same result.

### Pratt Parsers
TODO - details here