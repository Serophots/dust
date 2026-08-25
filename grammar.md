## Grammar

module         → item* EOF | mod ident block_expr;
item           → visibility? (
                  module | function
                ) ;

                  
function       → "fn" ident "()" block_expr ;

block_expr     → "{"  
                    (
                        statement*
                      | statement* expression_wo_block
                    )
                  "}" | ";" ;

statement      → ";"
               | item
               | let_stmt
               | (expression ";")   ;

let_stmt       → "let" ident ("=" expression )? ";"

expression     →  arithmetic
                | ident "=" expression
                | todo..
            (the block ones)
                | block_expr
                | if_expr
                | loop_expr ;


if_expr        → "if" expression block_expr
                ("else" (block_expr | if_expr) )? ;
                
loop_expr      → "loop" block_expr ;


arithmetic     → logic_or

logic_or       → logic_and ( "||" logic_and )* ;
logic_and      → equality ( "&&" equality )* ;

equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "*" ) unary )* ;

unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
               | "(" logic_or ")" ;
               

```
