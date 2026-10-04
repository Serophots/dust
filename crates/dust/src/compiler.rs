use std::{marker::PhantomData, ops::ControlFlow};

use camino::Utf8Path;
use dust_byt_interpret::VirtualMachine;
use dust_ctxt::{AstCtx, AstLowCtx, GblCtxt, HirCtx, WithContexts};
use miette::Result;

/// Any trait which implements this `Compiler`
/// trait can drive the compilation process.
pub trait Compiler<'gcx>: Sized {
    fn run<'byt>(self, root: &Utf8Path, gcx: GblCtxt<'gcx>) -> Result<Option<()>> {
        CompilerWrapper(self, PhantomData).run(root, gcx)
    }

    fn hook_ast<'ast, 'a>(
        &'a self,
        _ast: &'ast dust_ast::Krate<'gcx, 'ast>,
        _ctx: AstCtx<'ast, 'gcx>,
    ) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }

    fn hook_ast_lw<'ast, 'hir, 'a>(
        &'a self,
        _hir: &'hir dust_hir::Krate<'hir>,
        _ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> ControlFlow<()> {
        ControlFlow::Continue(())
    }
}

/// Any implementor of
struct CompilerWrapper<'gcx, T>(T, PhantomData<&'gcx ()>)
where
    T: Compiler<'gcx>;

impl<'gcx, T> WithContexts<'gcx> for CompilerWrapper<'gcx, T>
where
    T: Compiler<'gcx>,
{
    type RetAst<'ast>
        = &'ast dust_ast::Krate<'gcx, 'ast>
    where
        'gcx: 'ast;

    fn run_ast<'ast>(&self, ctx: AstCtx<'ast, 'gcx>) -> Result<&'ast dust_ast::Krate<'gcx, 'ast>> {
        Ok(dust_ast::parse_root(ctx)?)
    }

    fn hook_ast<'ast, 'a>(
        &'a self,
        ast: &'a Self::RetAst<'ast>,
        ctx: AstCtx<'ast, 'gcx>,
    ) -> ControlFlow<()>
    where
        'gcx: 'ast,
    {
        self.0.hook_ast(ast, ctx)
    }

    type RetAstLw<'hir>
        = &'hir dust_hir::Krate<'hir>
    where
        'gcx: 'hir;

    fn run_ast_lw<'ast, 'hir>(
        &self,
        krate: &'ast dust_ast::Krate<'gcx, 'ast>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> Result<&'hir dust_hir::Krate<'hir>> {
        Ok(dust_ast_lowering::lower_krate(krate, ctx)?)
    }

    fn hook_ast_lw<'ast, 'hir, 'a>(
        &'a self,
        ast: &'a Self::RetAstLw<'hir>,
        ctx: AstLowCtx<'ast, 'hir, 'gcx>,
    ) -> ControlFlow<()>
    where
        'gcx: 'hir,
    {
        self.0.hook_ast_lw(ast, ctx)
    }

    type RetHir<'hir> = &'hir dust_hir::Krate<'hir>;

    fn run_hir<'hir>(
        &self,
        hir: Self::RetAstLw<'hir>,
        _ctx: HirCtx<'hir, 'gcx>,
    ) -> Result<Self::RetHir<'hir>> {
        Ok(hir)
    }

    type RetHirLw<'byt> = dust_byt::Krate<'byt>;

    fn run_hir_lw<'hir, 'byt>(
        &self,
        hir: Self::RetHir<'hir>,
        ctx: dust_ctxt::HirLowCtx<'hir, 'byt, 'gcx>,
    ) -> Result<Self::RetHirLw<'byt>> {
        let chunk = dust_byt_comp::comp_krate(hir, ctx)?;

        Ok(chunk)
    }

    fn run_byt<'byt>(&self, krate: Self::RetHirLw<'byt>, ctx: dust_ctxt::BytCtx<'byt, 'gcx>) {
        dust_byt_print::print_krate(&krate, ctx.gcx);
        // let chunk = Func::from(&chunk);

        println!("---- interpretting!");

        let mut vm = VirtualMachine::new(&krate.main);
        vm.exec_chunk(&krate.main);

        dust_byt_print::print_stack(&vm);
    }
}
