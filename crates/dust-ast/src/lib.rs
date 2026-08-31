#![feature(allocator_api)]
use ahash::HashMap;
use camino::Utf8Path;
use dust_ctxt::AstCtx;
use miette::Result;

mod arithmetic;
mod parser;
mod primitive;
mod visitors;

#[cfg(test)]
mod tests;

pub use arithmetic::*;
pub use parser::*;
pub use primitive::*;

pub struct ParsedAst<'ast> {
    root: &'ast mut Module<'ast>,
    sub: HashMap<Path<'ast>, &'ast mut Module<'ast>>,
}

pub fn parse_module<'ast, 'gcx>(
    root: &Utf8Path,
    ctx: AstCtx<'ast, 'gcx>,
) -> Result<ParsedAst<'ast>> {
    // Parse root
    let source = ctx.arena.alloc(
        // TODO: Does this alloc on the heap, then move into the arena?
        std::fs::read_to_string(root).unwrap(),
    );
    let root = Parser::new(&source, ctx).mod_file(ctx)?;

    // Parse referenced modules

    for item in root.items {
        match item.r#type {
            ItemType::Use(_) => todo!(),
            ItemType::Module(module) => todo!(),
            ItemType::Function(_) => {}
        }
    }

    todo!()
    // Ok(ParsedAst { root, sub: () })
}
