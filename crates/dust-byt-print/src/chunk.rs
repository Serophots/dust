use colored::Colorize;
use dust_ctxt::GblCtxt;

pub fn print_krate(krate: &dust_byt::Krate, ctx: GblCtxt) {
    for &func in krate.funcs.iter() {
        print_func(func, krate, ctx);
        print!("\n\n")
    }
}

pub fn print_func(func: &dust_byt::Func, krate: &dust_byt::Krate, ctx: GblCtxt) {
    let idx = krate.funcs.iter().position(|f| f.id == func.id).unwrap();

    let name = ctx.symbols.resolve(func.ident.symbol).unwrap();
    let func_sig = format!("{} {}{}", "fn".magenta(), name.blue(), "()".white());
    let details = format!("({:?}, Idx({}))", func.id, idx).white();

    println!("{:<20} {}", func_sig, details);

    for (i, &instr) in func.instrs.iter().enumerate() {
        instr.decode().print(i, func, ctx);
    }

    println!("{}", "end".magenta());
}
