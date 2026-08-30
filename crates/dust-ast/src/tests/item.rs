use dust_ctxt::{create_and_enter_ast_ctxt, create_and_enter_global_ctxt};

use crate::Parser;

#[test]
fn test_item() {
    let () = create_and_enter_global_ctxt(|ctx| {
        let () = create_and_enter_ast_ctxt(ctx, |ctx| {
            let test_script = include_str!("../../../../assets/tests/ast-parser/item.dst");
            let module = Parser::new(test_script, ctx).mod_file(ctx).unwrap();

            insta::assert_json_snapshot!(module);
        });
    });
}
