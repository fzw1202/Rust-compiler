use std::io::{self};

#[derive(Debug)]
pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn dump(&self, s: &mut String) -> io::Result<()> {
        self.func_def.dump(s)?;
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
    fn dump(&self, s: &mut String) -> io::Result<()> {
        s.push_str(&format!("fun @{}(): {} {{\n", self.ident, self.func_type.dump()));
        self.block.dump(s)?;
        s.push_str("}\n");
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
    fn dump(&self, s: &mut String) -> io::Result<()> {
        s.push_str("%entry:\n");
        self.stmt.dump(s)?;
        Ok(())
    }
}

#[derive(Debug)]
pub struct Stmt {
    pub num: i32,
}

impl Stmt {
    fn dump(&self, s: &mut String) -> io::Result<()> {
        s.push_str(&format!("  ret {}\n", self.num));
        Ok(())
    }
}
