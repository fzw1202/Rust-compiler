use std::fs::File;
use std::io::{self, Write};

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn dump(&self, file: &mut File) -> io::Result<()> {
        self.func_def.dump(file)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    fn dump(&self, file: &mut File) -> io::Result<()> {
        writeln!(file, "fun @{}(): {} {{", self.ident, self.func_type.dump())?;
        self.block.dump(file)?;
        writeln!(file, "}}")?;
        Ok(())
    }
}

#[derive(Debug)]
pub enum FuncType {
    Int,
}

impl FuncType {
    fn dump(&self) -> String {
        match self {
            FuncType::Int => "i32".to_string(),
        }
    }
}

#[derive(Debug)]
pub struct Block {
    pub stmt: Stmt,
}

impl Block {
    fn dump(&self, file: &mut File) -> io::Result<()> {
        writeln!(file, "%entry:")?;
        self.stmt.dump(file)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}

impl Stmt {
    fn dump(&self, file: &mut File) -> io::Result<()> {
        writeln!(file, "  ret {}", self.num)?;
        Ok(())
    }
}
