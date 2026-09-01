# dust
An ambitious WIP learning project to build an interpretter for a Rust inspired syntax language.
I'm hopeful to dig into adding a type system of sorts.

I'd like to do so without bringing in too many dependencies.

## Progress:
- A basic arithmetic calculator / logic machine, handling equality, inequality, add/sub, mul/div, not, numbers, strings, bools, nil, obeying order of operations, parenthesis. Try `cargo run --bin dust-compiler -- calculate "1 - 2 * 3 >= -5 == true"`.
- Label your `.dst` input with lexer-level labelling `cargo run --bin dust-compiler -- lex ./assets/test.dst`.
- Label your `.dst` input with ast-level labelling `cargo run --bin dust-compiler -- parse ./assets/test.dst`.
- Output the AST tree `cargo run --bin dust-compiler -- parse --tree ./assets/test.dst`.

## Architecture
- `dust-lexer ` lexes text into a stream of basic `TokenKind`s
- `dust-ast` parses this stream into an abstract syntax tree, `Module`
- `dust-hir` parses the ast into a high level intermediate representation:
  - name & visibility resolution (variables, functions)
  - collate referenced dust (`.dst`) files
  - expand syntactic sugar
- The Abstract Syntax Tree

## Sources:
- https://matklad.github.io/2020/04/13/simple-but-powerful-pratt-parsing.html
- https://sdiehl.github.io/typechecker-zoo
- https://doc.rust-lang.org/reference/items.html
