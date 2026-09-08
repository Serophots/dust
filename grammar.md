## Grammar
```
module         → item* EOF | mod ident "{" item* "}";
item           → visibility? (
                  module | function | "use" path ";"
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


expression     →  
                      ------------------ Expression Call -------------------
                |   logic_or "()"
                      ------------------ Expression Or -------------------
                |   logic_and ( "||" logic_and )*
                      ------------------ Expression And -------------------
                |   equality ( "&&" equality )*
                      ------------------ Expression Equality -------------------
                |   comparison ( ( "!=" | "==" ) comparison )*
                      ------------------ Expression Comparison -------------------
                |   term ( ( ">" | ">=" | "<" | "<=" ) term )*
                      ------------------ Expression Term -------------------
                |   factor ( ( "-" | "+" ) factor )*
                      ------------------ Expression Factor -------------------
                |   unary ( ( "/" | "*" ) unary )*
                      ------------------ Expression Unarys -------------------
                |   ( "!" | "-" ) unary | parenth
                      ------------------ Expression Parenth -------------------
                | "(" expression ")"
                      ------------------ Expression Primarys -------------------
                | path                      
                | literal
                | ident "=" expression        
                | block_expr
                | if_expr
                | loop_expr ;

path           → ident ( "::" ident )*  ;

if_expr        → "if" expression block_expr
                ("else" (block_expr | if_expr) )? ;
                
loop_expr      → "loop" block_expr ;

literal        → NUMBER | STRING | "true" | "false" | "nil" ;
```
