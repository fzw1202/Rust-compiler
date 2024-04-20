use std::io::{self};

static mut CNT: i32 = 0;

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
        let a = self.exp.dump(s);
        s.push_str(&format!("  ret %{}\n", a));
        Ok(())
    }
}

pub struct Exp {
    pub loexp: LOrExp,
}

impl Exp {
    fn dump(&self, s: &mut String) -> i32 {
        self.loexp.dump(s)
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
            PrimaryExp::Number(ref num) => {
                unsafe {
                    CNT += 1;
                    s.push_str(&format!("  %{} = sub {}, 0\n", CNT, num));
                    return CNT;
                };
            },
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
                let a = uexp.dump(s);
                unsafe {
                    match uop {
                        UnaryOp::Pos => return a,
                        UnaryOp::Neg => {
                            CNT += 1;
                            s.push_str(&format!("  %{} = sub 0, %{}\n", CNT, a));
                            return CNT;
                        },
                        UnaryOp::Not => {
                            CNT += 1;
                            s.push_str(&format!("  %{} = eq %{}, 0\n", CNT, a));
                            return CNT;
                        },
                    };
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

pub enum MulOp {
    Mul,
    Div,
    Mod,
}

pub enum AddOp {
    Add,
    Sub,
}

pub enum MulExp {
    Unary(UnaryExp),
    Mul(Box<MulExp>, MulOp, UnaryExp),
}

impl MulExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            MulExp::Unary(uexp) => uexp.dump(s),
            MulExp::Mul(mexp, mop, uexp) => {
                let a: i32 = mexp.dump(s);
                let b: i32 = uexp.dump(s);
                unsafe {
                    CNT += 1;
                    match mop {
                        MulOp::Div => s.push_str(&format!("  %{} = div %{}, %{}", CNT, a, b)),
                        MulOp::Mod => s.push_str(&format!("  %{} = mod %{}, %{}", CNT, a, b)),
                        MulOp::Mul => s.push_str(&format!("  %{} = mul %{}, %{}", CNT, a, b)),
                    };
                    return CNT;
                }
            }
        }
    }
}

pub enum AddExp {
    Mul(MulExp),
    Add(Box<AddExp>, AddOp, MulExp),
}

impl AddExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            AddExp::Mul(mexp) => mexp.dump(s),
            AddExp::Add(aexp, aop, mexp) => {
                let a: i32 = aexp.dump(s);
                let b: i32 = mexp.dump(s);
                unsafe { 
                    CNT += 1; 
                    match aop {
                        AddOp::Add => s.push_str(&format!("  %{} = add %{}, %{}", CNT, a, b)),
                        AddOp::Sub => s.push_str(&format!("  %{} = sub %{}, %{}", CNT, a, b)),
                    };
                    return CNT;
                }
            },
        }
    }
}

pub enum RelOp {
    Less,
    Greater,
    LessEq,
    GreaterEq,
}

pub enum RelExp {
    Add(AddExp),
    Rel(Box<RelExp>, RelOp, AddExp),
}

impl RelExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            RelExp::Add(aexp) => aexp.dump(s),
            RelExp::Rel(rexp, rop, aexp) => {
                let a: i32 = rexp.dump(s);
                let b: i32 = aexp.dump(s);
                unsafe {
                    CNT += 1;
                    match rop {
                        RelOp::Less => s.push_str(&format!("  %{} = lt %{}, %{}", CNT, a, b)),
                        RelOp::Greater => s.push_str(&format!("  %{} = gt %{}, %{}", CNT, a, b)),
                        RelOp::LessEq => s.push_str(&format!("  %{} = le %{}, %{}", CNT, a, b)),
                        RelOp::GreaterEq => s.push_str(&format!("  %{} = ge %{}, %{}", CNT, a, b)),
                    };
                    return CNT;
                }
            },
        }
    }
}

pub enum EqOp {
    Eq,
    Neq,
}

pub enum EqExp {
    Rel(RelExp),
    Eq(Box<EqExp>, EqOp, RelExp),
}

impl EqExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            EqExp::Rel(rexp) => rexp.dump(s),
            EqExp::Eq(eexp, eop, rexp) => {
                let a: i32 = eexp.dump(s);
                let b: i32 = rexp.dump(s);
                unsafe {
                    CNT += 1;
                    match eop {
                        EqOp::Eq => s.push_str(&format!("  %{} = eq %{}, %{}", CNT, a, b)),
                        EqOp::Neq => s.push_str(&format!("  %{} = ne %{}, %{}", CNT, a, b)),
                    }
                    return CNT;
                }
            },
        }
    }
}

pub enum LAndExp {
    Eq(EqExp),
    LAnd(Box<LAndExp>, EqExp),
}

impl LAndExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            LAndExp::Eq(eexp) => eexp.dump(s),
            LAndExp::LAnd(laexp, eexp) => {
                let a: i32 = laexp.dump(s);
                let b: i32 = eexp.dump(s);
                unsafe {
                    s.push_str(&format!("  %{} = eq %{}, 0", CNT + 1, a));
                    s.push_str(&format!("  %{} = eq %{}, 0", CNT + 2, b));
                    s.push_str(&format!("  %{} = or %{}, %{}", CNT + 3, CNT + 1, CNT + 2));
                    s.push_str(&format!("  %{} = eq 0, %{}", CNT + 4, CNT + 3));
                    CNT += 4;
                    return CNT;
                }
            },
        }
    }
}

pub enum LOrExp {
    LAnd(LAndExp),
    LOr(Box<LOrExp>, LAndExp),
}

impl LOrExp {
    fn dump(&self, s: &mut String) -> i32 {
        match self {
            LOrExp::LAnd(laexp) => laexp.dump(s),
            LOrExp::LOr(loexp, laexp) => {
                let a: i32 = loexp.dump(s);
                let b: i32 = laexp.dump(s);
                unsafe {
                    s.push_str(&format!("  %{} = ne %{}, 0", CNT + 1, a));
                    s.push_str(&format!("  %{} = ne %{}, 0", CNT + 2, b));
                    s.push_str(&format!("  %{} = or %{}, %{}", CNT + 3, CNT + 1, CNT + 2));
                    CNT += 3;
                    return CNT;
                }
            },
        }
    }
}