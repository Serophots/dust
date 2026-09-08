# dust
An ambitious WIP learning project to build an interpretter for a Rust inspired syntax language.
I'm hopeful to dig into adding a type system of sorts.

I'd like to do so without bringing in too many dependencies.

## Progress:
- A basic arithmetic calculator / logic machine, handling equality, inequality, add/sub, mul/div, not, numbers, strings, bools, nil, obeying order of operations, parenthesis. Try `cargo run --bin dust-compiler -- calculate "1 - 2 * 3 >= -5 == true"`.
- Label your `.dst` input with lexer-level labelling `cargo run --bin dust -- lex ./assets/test.dst`.
- Label your `.dst` input with ast-level labelling `cargo run --bin dust -- parse ./assets/test.dst`.
- Output the AST tree `cargo run --bin dust -- parse --tree ./assets/test.dst`.

## Architecture

### Parsing
- `dust-lexer ` lexes text into a stream of basic `TokenKind`s
- `dust-ast` parses this stream into an abstract syntax tree, `Module`
- `dust-hir` parses the ast into a high level intermediate representation:
  - name & visibility resolution (variables, functions)
  - collate referenced dust (`.dst`) files
  - expand syntactic sugar

I'm writing Dust into these various representations to allow myself to experiment with various analyses in the future. I'm aware that at the minute they are premature optimisations.

### Bytecode
I'm implementing Dust with a register-based bytecode virtual machine, heavily inspired by Lua 5.0


## Sources:
- [Simple but powerful pratt parsing](https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html)
- [Rust Reference: Items](https://doc.rust-lang.org/reference/items.html)
- [Crafting Interpreters](https://craftinginterpreters.com/chunks-of-bytecode.html)
- [Lua 5.0](https://www.lua.org/doc/jucs05.pdf)
- [Typechecker zoo](https://sdiehl.github.io/typechecker-zoo)
- [Assignment as an expression](https://users.rust-lang.org/t/warn-about-using-the-value-of-an-assignment-expression/31324/5)
