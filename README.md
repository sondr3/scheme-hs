# `scheme-hs`

This was my term project for [INF225](https://www.uib.no/en/course/INF225) at UiB where we had to implement
something related to programming languages and parsing/creation. I decided to create a small R7RS Scheme
implementation, and to try and get as far as I could with my knowledge with Haskell and what I'd picked up
in INF225. This is the result, a yet-to-be-named implementation of the
[Scheme programming language](https://en.wikipedia.org/wiki/Scheme_%28programming_language%29)

## How to use

You need to have a Haskell environment with Cabal install, then clone this repository and run
`cabal run scheme` to be dropped into the REPL or `cabal run scheme -- <filename.scm>` to run a
Scheme program.

## Differences to R7RS

I managed to implement a fair bit of the standard, though I'm missing a whole bunch standard library
functionality. There are of course a lot of things that I haven't gotten around to implementing (yet)
and some things that might be slightly different from the R7RS standard. The biggest are:

- The `Complex` number type is defined as being a `Complex Double`, meaning that e.g. `(real? 2+0.0i)` will be truthy.
- No continuations
- Creating new syntax constructs is not yet supported

## LICENSE

MIT.
