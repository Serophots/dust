# dust
An ambitious WIP learning project to build an interpretter for a Rust inspired syntax language.
I'm hopeful to dig into adding a type system of sorts.

I'd like to do so without bringing in too many dependencies.

## Progress:
- A basic arithmetic calculator / logic machine, handling equality, inequality, add/sub, mul/div, not, numbers, strings, bools, nil, obeying order of operations, parenthesis. Try `cargo run --bin dust -- calculate "1 - 2 * 3 >= -5 == true"`.
- Label your `.dst` input with lexer-level labelling `cargo run --bin dust -- lex ./assets/test.dst`.
- Label your `.dst` input with ast-level labelling `cargo run --bin dust -- parse ./assets/test.dst`.
- Output the AST tree `cargo run --bin dust -- parse --tree ./assets/test.dst`.

## Architecture

### Overview
I'm writing Dust into these various representations to allow myself to experiment with various analyses in the future. I'm aware that at the minute they are premature optimisations.

- `dust-lexer` lexes text into a stream of basic `TokenKind`s
- `dust-ast` parses this stream into an abstract syntax tree
- `dust-hir` parses the ast into a high level intermediate representation:
  - name & visibility resolution (variables, functions)
  - collate referenced dust (`.dst`) files
  - expand syntactic sugar
- `dust-byt-comp` compiles the HIR into bytecode.

- `dust-byt-intrepret` interprets bytecode chunks

The two binary targets tie this chain of internal crates into a command line interface:
- `dust` provides all dust language functionality (compiling & interpretting)
- `dust-interpretter` implements the virtual machine interpretter only, as a slimmer binary than the full `dust`.

Each intermediate representation exhibits its own memory arena, and symbols are globally interned along the way.

### Bytecode
I'm implementing Dust with a register-based bytecode virtual machine, heavily inspired by Lua 5.0


## Sources:
- [Simple but powerful pratt parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
- [Rust Reference: Items](https://doc.rust-lang.org/reference/items.html)
- [Crafting Interpreters](https://craftinginterpreters.com/chunks-of-bytecode.html)
- [Lua 5.0](https://www.lua.org/doc/jucs05.pdf)
- [Typechecker zoo](https://sdiehl.github.io/typechecker-zoo)
- [Assignment as an expression](https://users.rust-lang.org/t/warn-about-using-the-value-of-an-assignment-expression/31324/5)
