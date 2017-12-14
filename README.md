# slox

`slox` is a Scala implementation of an interpreter for the Lox language
used in the [Crafting Interpreters][ci] book.  I decided that following
along and using Java (like the book does) would be too easy (heh), and I
wanted to write the interpreter in Scala, in an idiomatic (functional,
immutable data, type-safer, `null`-free, exception-free, etc.) way.

I did a quick search and didn't find any other Scala implementations,
though admittedly it was a *very* quick search.

## Usage

### Building

Grab the [sbt launcher script][sbtl] and put it somewhere in your
`PATH`.  Then run:

```
make assembly
```

### Running

To run the REPL, just run:

```
make run
```

To run a `.lox` script, run:

```
make run SCRIPT=/path/to/script.lox
```

## Deviations

There are a few differences between my version of Lox and the
'canonical' one.

### Bad

* The parser and interpreter use a lot of recursion.  Some of the
  functions in question are able to make use of tail-call optimization,
  but many calls are not.  Stack overflows may be possible while running
  certain Lox programs.
* This actually isn't a deviation, but it's "bad" from my perspective:
  so far I've implemented LoxInstance (for class instances) using a
  mutable data member (*gasp*).  I'll get around to finding an immutable
  soluion for that eventually that doesn't deviate too far from the
  script.

### Middling

* Instead of hand-writing the scanner, I decided to use Scala's
  [parser-combinators package][spc] to build one.  It seemed like a fun
  exercise to learn how to use it, though this means I missed out on
  writing my own.  I guess I'll get to dive into that when we get to the
  C version.

### Good

This is also a list of extras in the "CHALLENGES" section at the end of
each chapter that I've gotten around to implementing so far.

* I've implemented type coercions for concatenating strings with other
  types.
* I've made it a runtime error to divide by zero.
* My scanner preserves comments, and strips them after scanning but
  before parsing.  It even supports nested multiline comments.
* I've implemented error productions for attempts to use binary
  operators where a unary operator is expected.
* The `continue` and `break` keywords work inside `for` and `while`
  loops.
* I've implemented anonymous functions.
* It prints a warning for each unused local variable.
* Classes support static methods.  I deviated from the challenge in the
  book here and used `fun` inside a class body to denote a static
  method, instead of `class` as suggested.  Using `fun` just seems more
  natural to me, and also leaves open the possibility of using `class`
  inside a class body to support inner classes.
* Classes support 'getters' (property access that runs a block of code).
* I've implemented a fairly-easy way to build builtin classes, and added
  simple `List` and `File` classes, as well as a `System` class with
  some system-related functions.
* Typical of Scala programs, I eschew using `null` as the internal
  representation of Lox's `nil`, and instead repesent it as a distict
  type of literal.
* Since Scala has `case classes` and class-based pattern matching, I
  don't use any codegen for the expression/statement types, or use the
  visitor pattern.

## Thanks

Just wanted to give a quick note of thanks to Bob Nystrom, the author of
Crafting Interpreters.  I always thought PL and compiler/interpreter
development was a scary black art.  While I do think it's still
something of a black art, I'm beginning to see it's not all that scary.
His book is amazingly accessible and well written.  I'm looking forward
to the rest of the chapters, and highly recommend [reading it][ci] if
you're at all interested in this topic.

[ci]: https://www.craftinginterpreters.com/
[sbtl]: https://raw.githubusercontent.com/paulp/sbt-extras/master/sbt
[spc]: https://github.com/scala/scala-parser-combinators
