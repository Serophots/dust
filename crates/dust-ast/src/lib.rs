#![feature(allocator_api)]
#![feature(clone_from_ref)]
#![feature(str_as_str)]

use ahash::HashMap;
use camino::Utf8Path;
use dust_ctxt::AstCtx;
use miette::Result;
use utils::Box;

mod arithmetic;
mod module;
mod parser;
mod primitive;
pub mod visitors;

#[cfg(test)]
mod tests;

pub use arithmetic::*;
pub use module::*;
pub use parser::*;
pub use primitive::*;
pub use visitors::Visitor;

pub struct ParsedAst<'ast> {
    root: &'ast mut Module<'ast>,
    sub: HashMap<Path<'ast>, &'ast mut Module<'ast>>,
}

fn parse_file<'ast>(file: &Utf8Path, ctx: AstCtx<'ast, 'ast>) -> Result<&'ast mut Module<'ast>> {
    // TODO: When these nightly features land and some ergonomic work is done on the api,
    // maybe this can be both more efficient and ergonomic. For now, this works.
    let source = std::fs::read_to_string(file).unwrap().into_boxed_str();
    let source: Box<'ast, str> = Box::clone_from_ref_in(source.as_str(), ctx.arena);
    let source: &'ast mut str = Box::leak(source);
    Ok(Parser::<'ast>::new(source, ctx).mod_file(ctx)?)
}

pub fn parse_module<'ast, 'gcx>(
    root: &Utf8Path,
    ctx: AstCtx<'ast, 'gcx>,
) -> Result<&'ast ParsedAst<'ast>> {
    // Parse root
    let root = parse_file(root, ctx)?;

    Ok(ctx.arena.alloc(ParsedAst {
        root,
        sub: HashMap::default(),
    }))
}
