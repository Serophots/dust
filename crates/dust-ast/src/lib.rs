#![feature(allocator_api)]
#![feature(clone_from_ref)]
#![feature(str_as_str)]

use camino::Utf8Path;
use dust_ctxt::AstCtx;
use miette::Result;
use utils::{Box, Symbol};

mod module;
mod parser;
mod simplify;

pub mod visitors;

#[cfg(test)]
mod tests;

pub use parser::*;
pub use visitors::Visitor;

/// Many files are parsed into one large AST tree
/// which constitutes a Krate.
#[derive(PartialEq, serde::Serialize)]
pub struct Krate<'ast> {
    pub root: &'ast mut Module<'ast>,
}

fn read_to_string<'ast, 'gcx>(path: &Utf8Path, ctx: AstCtx<'ast, 'gcx>) -> &'ast mut str {
    // TODO: When these nightly features land and some ergonomic work is done on the api,
    // maybe this can be both more efficient and ergonomic. For now, this works.
    let source = std::fs::read_to_string(path).unwrap().into_boxed_str();
    let source: Box<'ast, str> = Box::clone_from_ref_in(source.as_str(), ctx.arena);
    let source: &'ast mut str = Box::leak(source);

    source
}

pub fn parse_root<'ast, 'gcx>(ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast Krate<'ast>> {
    let (root_ident, root_path) = ctx.expect_root();

    let source = read_to_string(root_path, ctx);
    let root = Parser::<'ast>::new(source, vec![*root_ident], ctx).parse(ctx)?;

    Ok(ctx.arena.alloc(Krate { root }))
}

/// Resolve a module (`&[Symbol]`) into an AST Module
fn parse_sub_module<'ast, 'gcx>(
    path: &[Symbol],
    ctx: AstCtx<'ast, 'gcx>,
) -> Result<&'ast Module<'ast>> {
    let filepath = parse_sub_path(path, ctx).ok_or_else(|| {
        miette::miette!(
            "Could not resolve the module {:?} into a valid dust file",
            path
        )
    })?;

    let source = read_to_string(filepath, ctx);

    Ok(Parser::<'ast>::new(source, Vec::from(path), ctx).parse(ctx)?)
}

/// Resolve a module (`&[Symbol]`) into its file path
fn parse_sub_path<'ast, 'gcx>(path: &[Symbol], ctx: AstCtx<'ast, 'gcx>) -> Option<&'ast Utf8Path> {
    let root = ctx.expect_root();

    if path.is_empty() {
        return Some(root.1);
    }

    let last = path.last().unwrap();
    let remaining = &path[0..path.len() - 1];

    let mut filepath = root.1.to_owned();
    filepath.pop();

    for x in remaining.iter().copied() {
        filepath.push(ctx.gcx.symbols.resolve(x).unwrap());
    }

    // Try, in order
    // 1. foo/bar/last_ident.dst
    {
        let mut filepath = filepath.clone();
        filepath.push(format!("{}.dst", ctx.gcx.symbols.resolve(*last).unwrap()));
        if filepath.is_file() {
            return Some(ctx.arena.alloc(filepath));
        }
    }

    // 2. foo/bar/last_ident/mod.dst
    {
        let mut filepath = filepath;
        filepath.push(ctx.gcx.symbols.resolve(*last).unwrap());
        filepath.push("mod.dst");
        if filepath.is_file() {
            return Some(ctx.arena.alloc(filepath));
        }
    }

    None
}
