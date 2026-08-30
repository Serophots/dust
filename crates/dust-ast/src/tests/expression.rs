use dust_ctxt::{create_and_enter_ast_ctxt, create_and_enter_global_ctxt};

use crate::Parser;

#[test]
fn test_lexer() {
    let () = create_and_enter_global_ctxt(|ctx| {
        let () = create_and_enter_ast_ctxt(ctx, |ctx| {
            let test_script = include_str!("../../../../assets/tests/ast-parser/expression.dst");
            let mut parser = Parser::new(test_script, ctx);
            let mut expressions = Vec::new();

            while let Ok(token) = parser.expression(ctx) {
                expressions.push(token);
            }

            insta::assert_json_snapshot!(expressions);
        });
    });
}
