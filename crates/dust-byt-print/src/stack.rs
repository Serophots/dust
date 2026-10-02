use colored::Colorize as _;
use dust_byt_interpret::VirtualMachine;
use utils::Lit;

pub fn print_stack(vm: &VirtualMachine) {
    println!(
        "{}{}",
        "stack".magenta(),
        format!("(len = {})", vm.stack.len()).white()
    );

    for (i, lit) in vm.stack.iter().enumerate() {
        let i = format!("{:<4}", i);

        let literal = match lit {
            Lit::Number(f) => format!("; {}", f),
            Lit::String(symbol) => {
                todo!()
                // format!("; \"{}\"", ctx.symbols.resolve(symbol).unwrap())
            }
            Lit::Bool(b) => match b {
                true => format!("; TRUE"),
                false => format!("; FALSE"),
            },
            Lit::Nil => format!("; NIL"),
        };

        println!("{:<4} {}", i.blue(), literal.yellow(),);
    }

    println!("{}", "end".magenta());
}
