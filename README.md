# flub-example-compiler

This project is an example compiler frontend for a straightforward,
statically-typed, functional language called Flub (functional Blub). It
should illustrate an architecture for "real-world" language demands (ranges,
error reporting, actual versus expected, etc.) while still being readable
over a weekend.

Flub is syntactically a mashup between ML, Scala, and Haskell. It supports
parametric polymorphism, higher-kinded types, type-in-type, recursive-let,
and not much else.

## Running the compiler

First, run the build:
```sh
spago build
```

Output formatted core:

```sh
node index.js example.flub
```

Normalize the "module" (the last declared expression):

```sh
node index.js example.flub -n
```

## Walkthrough

### Lexing

* [Syntax.Token](./src/Syntax/Token.purs)
* [Syntax.Lexer](./src/Syntax/Lexer.purs)

The lexer parses the non-recursive syntax (tokens). Lexical parsing is
generally very straightforward, but is where the majority of parsing time is
spent. Separating the language parser from the lexer, while optimizing the
lexer, is a great way to improve overall parsing performance.

This lexer is not optimized, but illustrates how one can implement a lazy
token stream with accurate source annotations for tokens. Lazy token streams
have a nice property of only parsing on demand, while also sharing work if
the language parser must backtrack.

### Parsing

* [Syntax.Tree](./src/Syntax/Tree.purs)
* [Syntax.Parser](./src/Syntax/Parser.purs)

The syntax tree is the concrete language syntax. Every token is represented
in the tree, and every token is annotated with positions, comments, and
whitespace. This means our original source is fully represented in this tree,
and can be printed back out exactly. This is a nice property since it means
we can implement transformations on input syntax while potentially retaining
the original formatting.

### Elaboration

* [Check.Core](./src/Check/Core.purs)
* [Check.Elaborate](./src/Check/Elaborate.purs)

The elaborator takes the concrete syntax, and transforms it into an internal
core language (known as [System Fω](https://en.wikipedia.org/wiki/System_F)).
In Core, all polymorphism is represented as explicit type-abstractions and
type-applications. Additionally, every term and argument is assigned an
explicit type. Unification variables are used to track unknown types, which
we later solve as part of type-checking.

While elaborating into Core, we also emit type-checking constraints, which
assert equalities between types. At specific points, we will then invoke the
solver, which processes these constraints and yields a substitution
(solutions) for our unknowns. Applying the substitution will yield Core
without any unknowns.

### Solving

* [Check.Solver](./src/Check/Solver.purs)
* [Check.Unify](./src/Check/Unify.purs)

The solver takes pending equality constraints from the elaborator and
attempts to solve them one-by-one through a process called unification.
Unification walks structurally over the two operands, and when a unification
variable meets some other type, records it as a solution. Types that are not
equivalent result in an error.

### Normalization

* [Eval.Normalize](./src/Eval/Normalize.purs)

Normalization reduces a term until it can't be reduced anymore. Generally,
System F is strongly normalizing (always reduces in finite time), but because
we allow recursive bindings, it's possible for normalization to loop forever.

In this implementation, it's done through a process called
normalization-by-evaluation. Terms are evaluated against their closure
environment, and then syntax is reifed from that environment.

### Printing

* [Print](./src/Print.purs)
* [Print.Precedence](./src/Print/Precedence.purs)

It's always nice to see our output in a human-readable manner. The most
confusing part about pretty-printing is handling fixity and precedence such
that parentheses are inserted in the appropriate places.

There are many ways to tackle this problem, but one of the more
straightforward ways is through an intermediate data structure that annotates
syntax with its fixity.
