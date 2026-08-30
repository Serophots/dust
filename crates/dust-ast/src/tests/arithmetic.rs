use dust_ctxt::{create_and_enter_ast_ctxt, create_and_enter_global_ctxt};

use crate::parser::Parser;

macro_rules! expand {
        ($(($name:ident, $lit:literal)),+ $(,)?) => {
            $(
                #[test]
                fn $name() {
                    let () = create_and_enter_global_ctxt(|ctx| {
                        let () = create_and_enter_ast_ctxt(ctx, |ctx| {
                            let parsed = Parser::new($lit, ctx).arithmetic(ctx).unwrap();
                            insta::assert_json_snapshot!(parsed);
                        });
                    });
                }
            )+
        };
    }

expand! {
    (assert_five, "5"),
    (assert_neg_five, "-5"),
    (assert_15_over_7, "3 * 5 / 7"),
    (assert_neg_35_over_7, "-7 * 5 / 7"),
    (assert_neg_7_plus_35, "-7 + 5 * 7"),
    (assert_10_over_7, "(-3 + 5) * 5 / 7"),
    (assert_1_sub_6, "1 - 2 * 3"),
    (assert_neg_5_lt_4, "1 - 2 * 3 < 4"),
    (assert_neg_5_gr_4, "1 - 2 * 3 > 4"),
    (assert_neg_5_lte_neg_5, "1 - 2 * 3 <= -5"),
    (assert_neg_5_gte_neg_5, "1 - 2 * 3 >= -5"),
    (assert_neg_5_gte_neg_5_eq_true, "1 - 2 * 3 >= -5 == true"),
    (assert_neg_5_gte_neg_5_eq_false, "1 - 2 * 3 >= -5 == false"),
    (assert_neg_5_gte_neg_5_neq_true, "1 - 2 * 3 >= -5 != true"),
    (assert_neg_5_gte_neg_5_neq_false, "1 - 2 * 3 >= -5 != false"),
    (assert_half_eq_half, "(1 / 2) == (1 / 2)"),
    (assert_zero_over_zero, "(0 / 0)"),
    (assert_zero_over_zero_eq_zero_over_zero, "(0 / 0) == (0 / 0)"),
    (assert_true_and_true, "true && true"),
    (assert_true_and_false, "true && false"),
    (assert_false_and_false, "false && false"),
    (assert_false_or_false, "false || false"),
    (assert_true_or_false, "true || false"),
    (assert_true_or_true, "true || true"),
    (assert_2_eq_2_and_true, "(4/2==2) && true"),
    (assert_2_eq_true_and_five_lt_three, "(4/2==2) && (5<3)"),
}
