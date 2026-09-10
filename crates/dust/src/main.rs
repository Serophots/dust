use dust::{Args, main_in_gbl_ctx};
use dust_ctxt::create_and_enter_global_ctxt;

fn main() -> miette::Result<()> {
    let args = <Args as clap::Parser>::parse();
    create_and_enter_global_ctxt(|ctx| main_in_gbl_ctx(args, ctx))
}
