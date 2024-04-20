use std::io::{self};

pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn dump(&self, s: &mut String) -> io::Result<()> {
        self.func_def.dump(s)?;
        Ok(())
    }
}

pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    fn dump(&self, s: &mut String) -> io::Result<()> {
        s.push_str(&format!(
            "fun @{}(): {} {{\n",
            self.ident,
            self.func_type.dump()
        ));
        self.block.dump(s)?;
        s.push_str("}\n");
        Ok(())
    }
}

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

pub struct Stmt {
    pub exp: Exp,
}

impl Stmt {
    fn dump(&self, s: &mut String) -> io::Result<()> {
        let cnt: i32 = self.exp.dump(s);
        if cnt >= 0 {
            s.push_str(&format!("  ret %{}\n", cnt));
        }
        else {
            s.push_str(&format!("  ret {}\n", -cnt))
        }
        Ok(())
    }
}

pub struct Exp {
    pub uexp: UnaryExp,
}

impl Exp {
    fn dump(&self, s: &mut String) -> i32 {
        self.uexp.dump(s)
    }
}

pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
}

impl PrimaryExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.dump(s),
            PrimaryExp::Number(ref num) => -num - 1,
        }
    }
}

pub enum UnaryExp {
    Primary(PrimaryExp),
    Unary(UnaryOp, Box<UnaryExp>),
}

impl UnaryExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            UnaryExp::Primary(pexp) => pexp.dump(s),
            UnaryExp::Unary(uop, uexp) => {
                let x = uexp.dump(s);
                if x < 0 {
                    match uop {
                        UnaryOp::Pos => return x + 1,
                        UnaryOp::Neg => s.push_str(&format!("  %{} = sub 0, {}\n", 0, -x - 1)),
                        UnaryOp::Not => s.push_str(&format!("  %{} = eq {}, 0\n", 0, -x - 1)),
                    };
                    return 0;
                } else {
                    match uop {
                        UnaryOp::Pos => return x,
                        UnaryOp::Neg => s.push_str(&format!("  %{} = sub 0, %{}\n", x + 1, x)),
                        UnaryOp::Not => s.push_str(&format!("  %{} = eq %{}, 0\n", x + 1, x)),
                    };
                    return x + 1;
                }
            }
        }
    }
}

pub enum UnaryOp {
    Pos,
    Neg,
    Not,
}
