use camino::Utf8PathBuf;
use dust::compiler::Compiler as _;
use dust_ctxt::create_and_enter_global_ctxt;
use miette::Result;

pub fn workspace_dir() -> Utf8PathBuf {
    use std::path::Path;

    let output = std::process::Command::new(env!("CARGO"))
        .arg("locate-project")
        .arg("--workspace")
        .arg("--message-format=plain")
        .output()
        .unwrap()
        .stdout;
    let cargo_path = Path::new(std::str::from_utf8(&output).unwrap().trim());
    Utf8PathBuf::from_path_buf(cargo_path.parent().unwrap().to_path_buf()).unwrap()
}

pub struct Compiler;

impl<'gcx> dust::compiler::Compiler<'gcx> for Compiler {
    fn hook_ast<'ast, 'a>(&'a self, ast: &'ast dust_ast::Krate<'ast>) -> std::ops::ControlFlow<()> {
        insta::assert_json_snapshot!(ast);

        std::ops::ControlFlow::Break(())
    }
}

#[test]
fn test_mod() -> Result<()> {
    let dir = {
        let mut dir = workspace_dir();
        dir.push("assets/tests/ast-parser/mod.dst");
        dir
    };

    create_and_enter_global_ctxt(|ctx| {
        Compiler.run(&dir, ctx)?;
        Ok(())
    })
}
