use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
// use std::fs::File;
use std::io::Result;

mod asm;
use asm::GenerateAsm;

mod ast;

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    let mut args = args();
    args.next();
    let mode = args.next().unwrap();

    let input = args.next().unwrap();
    let input = read_to_string(input)?;
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    args.next();
    let output = args.next().unwrap();
    let mut s = String::from("");
    ast.dump(&mut s)?;

    if mode == "-koopa" {
        println!("koopa");
        std::fs::write(output, &s)?;
    } else if mode == "-riscv" {
        println!("riscv");
        let driver = koopa::front::Driver::from(s);
        let program = driver.generate_program().unwrap();

        let mut asm = String::from("");
        program.generate(&mut asm, None);
        std::fs::write(output, &asm)?;
    }
    Ok(())
}
