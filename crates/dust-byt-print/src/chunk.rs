use colored::Colorize;
use dust_byt::Instruction;
use dust_ctxt::GblCtxt;
use utils::Lit;

pub fn print_krate(krate: &dust_byt::Krate, ctx: GblCtxt) {
    for func in krate.funcs.iter() {
        print_func(func, ctx);
        print!("\n\n")
    }
}

pub fn print_func(func: &dust_byt::Func, ctx: GblCtxt) {
    let name = ctx.symbols.resolve(func.ident.symbol).unwrap();
    println!(
        "{} {}{}",
        "function".magenta(),
        name.blue(),
        "(...)".white()
    );

    for (i, &instr) in func.instrs.iter().enumerate() {
        let instr = Instruction::from(instr);
        let i = format!("{:<4}", i + 1);

        let operands = match instr {
            dust_byt::Instruction::Abc { a, b, c, .. } => format!("{} {} {}", a, b, c),
            dust_byt::Instruction::ABx { a, bx, .. } => format!("{} {}", a, bx),
            dust_byt::Instruction::AsBx { a, sbx, .. } => format!("{} {}", a, sbx),
        };

        let comment = match instr {
            dust_byt::Instruction::Abc { operation, .. } => match operation {
                dust_byt::OpAbc::Move => None,
                dust_byt::OpAbc::Add => None,
                dust_byt::OpAbc::Sub => None,
                dust_byt::OpAbc::Mul => None,
                dust_byt::OpAbc::Div => None,
                dust_byt::OpAbc::Eq => None,
                dust_byt::OpAbc::NEq => None,
                dust_byt::OpAbc::Greater => None,
                dust_byt::OpAbc::GreaterEqual => None,
                dust_byt::OpAbc::Lesser => None,
                dust_byt::OpAbc::LesserEqual => None,
                dust_byt::OpAbc::And => None,
                dust_byt::OpAbc::Or => None,
                dust_byt::OpAbc::Call => None,
            },
            dust_byt::Instruction::ABx {
                operation,
                a: _,
                bx,
            } => match operation {
                dust_byt::OpABx::LoadK => {
                    let r#const = func.consts[bx as usize];

                    Some(match r#const {
                        Lit::Number(f) => format!("; {}", f),
                        Lit::String(symbol) => {
                            format!("; \"{}\"", ctx.symbols.resolve(symbol).unwrap())
                        }
                        Lit::Bool(b) => match b {
                            true => format!("; TRUE"),
                            false => format!("; FALSE"),
                        },
                        Lit::Nil => format!("; NIL"),
                    })
                }
                dust_byt::OpABx::LoadNil => Some(format!("; NIL")),
            },
            dust_byt::Instruction::AsBx { operation, .. } => match operation {
                dust_byt::OpAsBx::LoadF64 => None,
            },
        };

        print!(
            "{:<4} {:<12} {:<10}",
            i.blue(),
            instr.name().yellow(),
            operands.blue()
        );
        if let Some(comment) = comment {
            print!("{}", comment.bright_green())
        }
        print!("\n")
    }

    println!("{}", "end".magenta());
}
