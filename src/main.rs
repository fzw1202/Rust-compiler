use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
// use std::fs::File;
use std::io::Result;

mod asm;
use asm::GenerateAsm;

use koopa::ir::types;

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
    let mut scope = ast::Scopes::new();
    ast.ir(&mut s, &mut scope)?;

    if mode == "-koopa" {
        println!("koopa");
        std::fs::write(output, &s)?;
    } else if mode == "-riscv" {
        println!("riscv");
        let driver = koopa::front::Driver::from(s);
        types::Type::set_ptr_size(4);
        let program = driver.generate_program().unwrap();
        std::fs::write(output, program.generate(None, None, None, None))?;
    }
    Ok(())
}
