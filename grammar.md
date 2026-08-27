## Grammar

module         → item* EOF | mod ident "{" item* "}";
item           → visibility? (
                  module | function
                ) ;

                  
function       → "fn" ident "()" block_expr ;

block_expr     → "{"  
                    (
                        statement*
                      | statement* expression_w/o_block
                    )
                  "}" ;

statement      → ";"
               | item
               | let_stmt
               | (expression ";")   ;

let_stmt       → "let" ident ("=" expression )? ";"

expression     →  arithmetic
                | ident "=" expression
                | expression "()"
                | path
                | todo..
            (the block ones)
                | block_expr
                | if_expr
                | loop_expr ;

path           → ident ( "::" ident )*  ;

if_expr        → "if" expression block_expr
                ("else" (block_expr | if_expr) )? ;
                
loop_expr      → "loop" block_expr ;


arithmetic     → logic_or

logic_or       → logic_and ( "||" logic_and )* ;
logic_and      → equality ( "&&" equality )* ;

equality       → comparison ( ( "!=" | "==" ) comparison )* ;
comparison     → term ( ( ">" | ">=" | "<" | "<=" ) term )* ;

term           → factor ( ( "-" | "+" ) factor )* ;
factor         → unary ( ( "/" | "\*" ) unary )* ;

unary          → ( "!" | "-" ) unary
               | primary ;
primary        → NUMBER | STRING | "true" | "false" | "nil"
              | "(" arithmetic ")"
              | IDENTIFIER ;

```
