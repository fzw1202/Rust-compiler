use lalrpop_util::lalrpop_mod;
use std::env::args;
use std::fs::read_to_string;
use std::io::Result;

use std::fs::File;

mod ast;

lalrpop_mod!(sysy);

fn main() -> Result<()> {
    let mut args = args();
    args.next();
    let _mode = args.next().unwrap();

    let input = args.next().unwrap();
    let input = read_to_string(input)?;
    let ast = sysy::CompUnitParser::new().parse(&input).unwrap();

    args.next();
    let output = args.next().unwrap();
    let mut file = File::create(output)?;

    // println!("{:#?}", ast);

    ast.dump(&mut file)?;
    Ok(())
}
