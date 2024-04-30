use once_cell::sync::Lazy;
use std::{collections::HashMap, io};

#[derive(Clone)]
pub enum Symbol {
    Const(i32),
    Var(String),
}

static mut CNT: i32 = 0;
static mut SYMBOLS: Lazy<HashMap<String, Symbol>> = Lazy::new(|| HashMap::new());

pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn symbol(&self) -> io::Result<()> {
        self.func_def.symbol()?;
        Ok(())
    }

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
    fn symbol(&self) -> io::Result<()> {
        self.block.symbol()?;
        Ok(())
    }

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
    pub bitems: Vec<BlockItem>,
}

impl Block {
    fn symbol(&self) -> io::Result<()> {
        for bitem in &self.bitems {
            bitem.symbol()?;
        }
        Ok(())
    }

    fn dump(&self, s: &mut String) -> io::Result<()> {
        let mut cnt: i32 = 0;

        s.push_str("%entry:\n");
        for bitem in &self.bitems {
            bitem.dump(s)?;
            match bitem {
                BlockItem::Stm(stmt) => match stmt {
                    Stmt::Ret(_exp) => {
                        cnt += 1;
                        break;
                    }
                    _ => (),
                },
                _ => (),
            }
        }

        if cnt == 0 {
            s.push_str(&format!("  ret 0\n"));
        }
        Ok(())
    }
}

pub enum BlockItem {
    Dec(Decl),
    Stm(Stmt),
}

impl BlockItem {
    fn symbol(&self) -> io::Result<()> {
        match self {
            BlockItem::Dec(decl) => decl.symbol()?,
            BlockItem::Stm(_stmt) => (),
        };
        Ok(())
    }

    fn dump(&self, s: &mut String) -> io::Result<()> {
        match self {
            BlockItem::Dec(decl) => decl.dump(s)?,
            BlockItem::Stm(stmt) => stmt.dump(s)?,
        };
        Ok(())
    }
}

pub enum Decl {
    Const(ConstDecl),
    Var(VarDecl),
}

impl Decl {
    fn symbol(&self) -> io::Result<()> {
        match self {
            Decl::Const(cdecl) => cdecl.symbol()?,
            Decl::Var(vdecl) => vdecl.symbol()?,
        };
        Ok(())
    }

    fn dump(&self, s: &mut String) -> io::Result<()> {
        match self {
            Decl::Const(_cdecl) => (),
            Decl::Var(vdecl) => vdecl.dump(s)?,
        };
        Ok(())
    }
}

pub struct ConstDecl {
    pub btype: BType,
    pub cdefs: Vec<ConstDef>,
}

impl ConstDecl {
    fn symbol(&self) -> io::Result<()> {
        match self.btype {
            BType::INT => {
                for cdef in &self.cdefs {
                    cdef.symbol()?;
                }
            }
        };
        Ok(())
    }
}

pub struct VarDecl {
    pub btype: BType,
    pub vdefs: Vec<VarDef>,
}

impl VarDecl {
    fn symbol(&self) -> io::Result<()> {
        match self.btype {
            BType::INT => {
                for vdef in &self.vdefs {
                    vdef.symbol()?;
                }
            }
        };
        Ok(())
    }

    fn dump(&self, s: &mut String) -> io::Result<()> {
        match self.btype {
            BType::INT => {
                for vdef in &self.vdefs {
                    vdef.dump(s, &"i32".to_string())?;
                }
            }
        };
        Ok(())
    }
}

pub enum BType {
    INT,
}

pub struct ConstDef {
    pub ident: String,
    pub cinitval: ConstInitVal,
}

impl ConstDef {
    fn symbol(&self) -> io::Result<()> {
        let value: i32 = self.cinitval.symbol();
        unsafe {
            SYMBOLS.insert(self.ident.clone(), Symbol::Const(value));
        }
        Ok(())
    }
}

pub enum VarDef {
    Def(String),
    Ass(String, InitVal),
}

impl VarDef {
    fn symbol(&self) -> io::Result<()> {
        match self {
            VarDef::Def(name) => unsafe {
                SYMBOLS.insert(name.to_string(), Symbol::Var(format!("@_{}", name)));
            },
            VarDef::Ass(name, _initval) => unsafe {
                SYMBOLS.insert(name.to_string(), Symbol::Var(format!("@_{}", name)));
            },
        };
        Ok(())
    }

    fn dump(&self, s: &mut String, t: &String) -> io::Result<()> {
        match self {
            VarDef::Def(name) => unsafe {
                let varname = SYMBOLS.get(name).unwrap();
                match varname {
                    Symbol::Var(n) => s.push_str(&format!("  {} = alloc {}\n", &n, t)),
                    _ => (),
                };
            },
            VarDef::Ass(name, initval) => unsafe {
                let varname = SYMBOLS.get(name).unwrap();
                match varname {
                    Symbol::Var(n) => {
                        let result = initval.dump(s);
                        s.push_str(&format!("  {} = alloc {}\n", &n, t));
                        match result {
                            Ok(cnt) => {
                                s.push_str(&format!("  store %{}, {}\n", cnt, &n));
                            }
                            Err(value) => {
                                s.push_str(&format!("  store {}, {}\n", value, &n));
                            }
                        };
                    }
                    _ => (),
                };
            },
        };
        Ok(())
    }
}

pub struct ConstInitVal {
    pub cexp: ConstExp,
}

impl ConstInitVal {
    fn symbol(&self) -> i32 {
        self.cexp.symbol()
    }
}

pub struct InitVal {
    pub exp: Exp,
}

impl InitVal {
    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        self.exp.dump(s)
    }
}
pub struct ConstExp {
    pub exp: Exp,
}

impl ConstExp {
    fn symbol(&self) -> i32 {
        self.exp.symbol()
    }
}

pub struct LVal {
    pub ident: String,
}

impl LVal {
    fn get_value(&self) -> Symbol {
        unsafe { SYMBOLS.get(&self.ident).unwrap().clone() }
    }
}

pub enum Stmt {
    Ass(LVal, Exp),
    Ret(Exp),
}

impl Stmt {
    fn dump(&self, s: &mut String) -> io::Result<()> {
        match self {
            Stmt::Ass(lval, exp) => match lval.get_value() {
                Symbol::Const(_value) => {
                    panic!("assignment for const value is not allowed!")
                }
                Symbol::Var(name) => {
                    let result = exp.dump(s);
                    match result {
                        Ok(cnt) => s.push_str(&format!("  store %{}, {}\n", cnt, name)),
                        Err(value) => s.push_str(&format!("  store {}, {}\n", value, name)),
                    }
                }
            },
            Stmt::Ret(exp) => {
                let result = exp.dump(s);
                match result {
                    Ok(cnt) => s.push_str(&format!("  ret %{}\n", cnt)),
                    Err(value) => s.push_str(&format!("  ret {}\n", value)),
                }
            }
        };
        Ok(())
    }
}

pub struct Exp {
    pub loexp: LOrExp,
}

impl Exp {
    fn symbol(&self) -> i32 {
        self.loexp.symbol()
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        self.loexp.dump(s)
    }
}

pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
    LVal(Box<LVal>),
}

impl PrimaryExp {
    fn symbol(&self) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.symbol(),
            PrimaryExp::Number(ref num) => *num,
            PrimaryExp::LVal(lval) => match lval.get_value() {
                Symbol::Const(value) => value,
                Symbol::Var(_name) => {
                    panic!("can not use variables to assign const value!")
                }
            },
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            PrimaryExp::Exp(exp) => exp.dump(s),
            PrimaryExp::Number(num) => Err(*num),
            PrimaryExp::LVal(lval) => unsafe {
                match lval.get_value() {
                    Symbol::Const(value) => Err(value),
                    Symbol::Var(name) => {
                        CNT += 1;
                        s.push_str(&format!("  %{} = load {}\n", CNT, name));
                        Ok(CNT)
                    }
                }
            },
        }
    }
}

pub enum UnaryExp {
    Primary(PrimaryExp),
    Unary(UnaryOp, Box<UnaryExp>),
}

impl UnaryExp {
    fn symbol(&self) -> i32 {
        match self {
            UnaryExp::Primary(pexp) => pexp.symbol(),
            UnaryExp::Unary(uop, uexp) => {
                let a = uexp.symbol();
                match uop {
                    UnaryOp::Pos => a,
                    UnaryOp::Neg => -a,
                    UnaryOp::Not => (a == 0) as i32,
                }
            }
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            UnaryExp::Primary(pexp) => pexp.dump(s),
            UnaryExp::Unary(uop, uexp) => unsafe {
                let result = uexp.dump(s);
                match uop {
                    UnaryOp::Pos => result,
                    UnaryOp::Neg => {
                        CNT += 1;
                        match result {
                            Ok(cnt) => s.push_str(&format!("  %{} = sub 0, %{}\n", CNT, cnt)),
                            Err(value) => s.push_str(&format!("  %{} = sub 0, {}\n", CNT, value)),
                        };
                        Ok(CNT)
                    }
                    UnaryOp::Not => {
                        CNT += 1;
                        match result {
                            Ok(cnt) => s.push_str(&format!("  %{} = eq %{}, 0\n", CNT, cnt)),
                            Err(value) => s.push_str(&format!("  %{} = eq {}, 0\n", CNT, value)),
                        };
                        Ok(CNT)
                    }
                }
            },
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
    fn symbol(&self) -> i32 {
        match self {
            MulExp::Unary(uexp) => uexp.symbol(),
            MulExp::Mul(mexp, mop, uexp) => {
                let a: i32 = mexp.symbol();
                let b: i32 = uexp.symbol();
                match mop {
                    MulOp::Div => a / b,
                    MulOp::Mod => a % b,
                    MulOp::Mul => a * b,
                }
            }
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            MulExp::Unary(uexp) => uexp.dump(s),
            MulExp::Mul(mexp, mop, uexp) => unsafe {
                let l = mexp.dump(s);
                let r = uexp.dump(s);

                CNT += 1;
                match l {
                    Ok(lcnt) => {
                        match r {
                            Ok(rcnt) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!(
                                        "  %{} = div %{}, %{}\n",
                                        CNT, lcnt, rcnt
                                    )),
                                    MulOp::Mod => s.push_str(&format!(
                                        "  %{} = mod %{}, %{}\n",
                                        CNT, lcnt, rcnt
                                    )),
                                    MulOp::Mul => s.push_str(&format!(
                                        "  %{} = mul %{}, %{}\n",
                                        CNT, lcnt, rcnt
                                    )),
                                };
                            }
                            Err(rvalue) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!(
                                        "  %{} = div %{}, {}\n",
                                        CNT, lcnt, rvalue
                                    )),
                                    MulOp::Mod => s.push_str(&format!(
                                        "  %{} = mod %{}, {}\n",
                                        CNT, lcnt, rvalue
                                    )),
                                    MulOp::Mul => s.push_str(&format!(
                                        "  %{} = mul %{}, {}\n",
                                        CNT, lcnt, rvalue
                                    )),
                                };
                            }
                        };
                    }
                    Err(lvalue) => {
                        match r {
                            Ok(rcnt) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!(
                                        "  %{} = div {}, %{}\n",
                                        CNT, lvalue, rcnt
                                    )),
                                    MulOp::Mod => s.push_str(&format!(
                                        "  %{} = mod {}, %{}\n",
                                        CNT, lvalue, rcnt
                                    )),
                                    MulOp::Mul => s.push_str(&format!(
                                        "  %{} = mul {}, %{}\n",
                                        CNT, lvalue, rcnt
                                    )),
                                };
                            }
                            Err(rvalue) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!(
                                        "  %{} = div {}, {}\n",
                                        CNT, lvalue, rvalue
                                    )),
                                    MulOp::Mod => s.push_str(&format!(
                                        "  %{} = mod {}, {}\n",
                                        CNT, lvalue, rvalue
                                    )),
                                    MulOp::Mul => s.push_str(&format!(
                                        "  %{} = mul {}, {}\n",
                                        CNT, lvalue, rvalue
                                    )),
                                };
                            }
                        };
                    }
                };
                Ok(CNT)
            },
        }
    }
}

pub enum AddExp {
    Mul(MulExp),
    Add(Box<AddExp>, AddOp, MulExp),
}

impl AddExp {
    fn symbol(&self) -> i32 {
        match self {
            AddExp::Mul(mexp) => mexp.symbol(),
            AddExp::Add(aexp, aop, mexp) => {
                let a: i32 = aexp.symbol();
                let b: i32 = mexp.symbol();
                match aop {
                    AddOp::Add => a + b,
                    AddOp::Sub => a - b,
                }
            }
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            AddExp::Mul(mexp) => mexp.dump(s),
            AddExp::Add(aexp, aop, mexp) => unsafe {
                let l = aexp.dump(s);
                let r = mexp.dump(s);
                CNT += 1;

                match l {
                    Ok(lcnt) => {
                        match r {
                            Ok(rcnt) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!(
                                        "  %{} = add %{}, %{}\n",
                                        CNT, lcnt, rcnt
                                    )),
                                    AddOp::Sub => s.push_str(&format!(
                                        "  %{} = sub %{}, %{}\n",
                                        CNT, lcnt, rcnt
                                    )),
                                };
                            }
                            Err(rvalue) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!(
                                        "  %{} = add %{}, {}\n",
                                        CNT, lcnt, rvalue
                                    )),
                                    AddOp::Sub => s.push_str(&format!(
                                        "  %{} = sub %{}, {}\n",
                                        CNT, lcnt, rvalue
                                    )),
                                };
                            }
                        };
                    }
                    Err(lvalue) => {
                        match r {
                            Ok(rcnt) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!(
                                        "  %{} = add {}, %{}\n",
                                        CNT, lvalue, rcnt
                                    )),
                                    AddOp::Sub => s.push_str(&format!(
                                        "  %{} = sub {}, %{}\n",
                                        CNT, lvalue, rcnt
                                    )),
                                };
                            }
                            Err(rvalue) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!(
                                        "  %{} = add {}, {}\n",
                                        CNT, lvalue, rvalue
                                    )),
                                    AddOp::Sub => s.push_str(&format!(
                                        "  %{} = sub {}, {}\n",
                                        CNT, lvalue, rvalue
                                    )),
                                };
                            }
                        };
                    }
                };
                Ok(CNT)
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
    fn symbol(&self) -> i32 {
        match self {
            RelExp::Add(aexp) => aexp.symbol(),
            RelExp::Rel(rexp, rop, aexp) => {
                let a: i32 = rexp.symbol();
                let b: i32 = aexp.symbol();
                match rop {
                    RelOp::Less => (a < b) as i32,
                    RelOp::Greater => (a > b) as i32,
                    RelOp::LessEq => (a <= b) as i32,
                    RelOp::GreaterEq => (a >= b) as i32,
                }
            }
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            RelExp::Add(aexp) => aexp.dump(s),
            RelExp::Rel(rexp, rop, aexp) => unsafe {
                let l = rexp.dump(s);
                let r = aexp.dump(s);
                CNT += 1;
                match l {
                    Ok(lcnt) => match r {
                        Ok(rcnt) => {
                            match rop {
                                RelOp::Less => {
                                    s.push_str(&format!("  %{} = lt %{}, %{}\n", CNT, lcnt, rcnt))
                                }
                                RelOp::Greater => {
                                    s.push_str(&format!("  %{} = gt %{}, %{}\n", CNT, lcnt, rcnt))
                                }
                                RelOp::LessEq => {
                                    s.push_str(&format!("  %{} = le %{}, %{}\n", CNT, lcnt, rcnt))
                                }
                                RelOp::GreaterEq => {
                                    s.push_str(&format!("  %{} = ge %{}, %{}\n", CNT, lcnt, rcnt))
                                }
                            };
                        }
                        Err(rvalue) => {
                            match rop {
                                RelOp::Less => {
                                    s.push_str(&format!("  %{} = lt %{}, {}\n", CNT, lcnt, rvalue))
                                }
                                RelOp::Greater => {
                                    s.push_str(&format!("  %{} = gt %{}, {}\n", CNT, lcnt, rvalue))
                                }
                                RelOp::LessEq => {
                                    s.push_str(&format!("  %{} = le %{}, {}\n", CNT, lcnt, rvalue))
                                }
                                RelOp::GreaterEq => {
                                    s.push_str(&format!("  %{} = ge %{}, {}\n", CNT, lcnt, rvalue))
                                }
                            };
                        }
                    },
                    Err(lvalue) => match r {
                        Ok(rcnt) => {
                            match rop {
                                RelOp::Less => {
                                    s.push_str(&format!("  %{} = lt {}, %{}\n", CNT, lvalue, rcnt))
                                }
                                RelOp::Greater => {
                                    s.push_str(&format!("  %{} = gt {}, %{}\n", CNT, lvalue, rcnt))
                                }
                                RelOp::LessEq => {
                                    s.push_str(&format!("  %{} = le {}, %{}\n", CNT, lvalue, rcnt))
                                }
                                RelOp::GreaterEq => {
                                    s.push_str(&format!("  %{} = ge {}, %{}\n", CNT, lvalue, rcnt))
                                }
                            };
                        }
                        Err(rvalue) => {
                            match rop {
                                RelOp::Less => {
                                    s.push_str(&format!("  %{} = lt {}, {}\n", CNT, lvalue, rvalue))
                                }
                                RelOp::Greater => {
                                    s.push_str(&format!("  %{} = gt {}, {}\n", CNT, lvalue, rvalue))
                                }
                                RelOp::LessEq => {
                                    s.push_str(&format!("  %{} = le {}, {}\n", CNT, lvalue, rvalue))
                                }
                                RelOp::GreaterEq => {
                                    s.push_str(&format!("  %{} = ge {}, {}\n", CNT, lvalue, rvalue))
                                }
                            };
                        }
                    },
                }
                Ok(CNT)
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
    fn symbol(&self) -> i32 {
        match self {
            EqExp::Rel(rexp) => rexp.symbol(),
            EqExp::Eq(eexp, eop, rexp) => {
                let a: i32 = eexp.symbol();
                let b: i32 = rexp.symbol();
                match eop {
                    EqOp::Eq => (a == b) as i32,
                    EqOp::Neq => (a != b) as i32,
                }
            }
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            EqExp::Rel(rexp) => rexp.dump(s),
            EqExp::Eq(eexp, eop, rexp) => unsafe {
                let l = eexp.dump(s);
                let r = rexp.dump(s);
                CNT += 1;

                match l {
                    Ok(lcnt) => match r {
                        Ok(rcnt) => match eop {
                            EqOp::Eq => {
                                s.push_str(&format!("  %{} = eq %{}, %{}\n", CNT, lcnt, rcnt))
                            }
                            EqOp::Neq => {
                                s.push_str(&format!("  %{} = ne %{}, %{}\n", CNT, lcnt, rcnt))
                            }
                        },
                        Err(rvalue) => match eop {
                            EqOp::Eq => {
                                s.push_str(&format!("  %{} = eq %{}, {}\n", CNT, lcnt, rvalue))
                            }
                            EqOp::Neq => {
                                s.push_str(&format!("  %{} = ne %{}, {}\n", CNT, lcnt, rvalue))
                            }
                        },
                    },
                    Err(lvalue) => match r {
                        Ok(rcnt) => match eop {
                            EqOp::Eq => {
                                s.push_str(&format!("  %{} = eq {}, %{}\n", CNT, lvalue, rcnt))
                            }
                            EqOp::Neq => {
                                s.push_str(&format!("  %{} = ne {}, %{}\n", CNT, lvalue, rcnt))
                            }
                        },
                        Err(rvalue) => match eop {
                            EqOp::Eq => {
                                s.push_str(&format!("  %{} = eq {}, {}\n", CNT, lvalue, rvalue))
                            }
                            EqOp::Neq => {
                                s.push_str(&format!("  %{} = ne {}, {}\n", CNT, lvalue, rvalue))
                            }
                        },
                    },
                }
                Ok(CNT)
            },
        }
    }
}

pub enum LAndExp {
    Eq(EqExp),
    LAnd(Box<LAndExp>, EqExp),
}

impl LAndExp {
    fn symbol(&self) -> i32 {
        match self {
            LAndExp::Eq(eexp) => eexp.symbol(),
            LAndExp::LAnd(laexp, eexp) => {
                let a: i32 = laexp.symbol();
                let b: i32 = eexp.symbol();
                (a != 0 && b != 0) as i32
            }
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            LAndExp::Eq(eexp) => eexp.dump(s),
            LAndExp::LAnd(laexp, eexp) => unsafe {
                let l = laexp.dump(s);
                let r = eexp.dump(s);
                match l {
                    Ok(lcnt) => s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 1, lcnt)),
                    Err(lvalue) => s.push_str(&format!("  %{} = eq {}, 0\n", CNT + 1, lvalue)),
                };
                match r {
                    Ok(rcnt) => s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 2, rcnt)),
                    Err(rvalue) => s.push_str(&format!("  %{} = eq {}, 0\n", CNT + 2, rvalue)),
                };
                s.push_str(&format!("  %{} = or %{}, %{}\n", CNT + 3, CNT + 1, CNT + 2));
                s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 4, CNT + 3));
                CNT += 4;
                Ok(CNT)
            },
        }
    }
}

pub enum LOrExp {
    LAnd(LAndExp),
    LOr(Box<LOrExp>, LAndExp),
}

impl LOrExp {
    fn symbol(&self) -> i32 {
        match self {
            LOrExp::LAnd(laexp) => laexp.symbol(),
            LOrExp::LOr(loexp, laexp) => {
                let a: i32 = loexp.symbol();
                let b: i32 = laexp.symbol();
                (a != 0 || b != 0) as i32
            }
        }
    }

    fn dump(&self, s: &mut String) -> Result<i32, i32> {
        match self {
            LOrExp::LAnd(laexp) => laexp.dump(s),
            LOrExp::LOr(loexp, laexp) => unsafe {
                let l = loexp.dump(s);
                let r = laexp.dump(s);
                match l {
                    Ok(lcnt) => s.push_str(&format!("  %{} = ne %{}, 0\n", CNT + 1, lcnt)),
                    Err(lvalue) => s.push_str(&format!("  %{} = ne {}, 0\n", CNT + 1, lvalue)),
                };
                match r {
                    Ok(rcnt) => s.push_str(&format!("  %{} = ne %{}, 0\n", CNT + 2, rcnt)),
                    Err(rvalue) => s.push_str(&format!("  %{} = ne {}, 0\n", CNT + 2, rvalue)),
                };
                s.push_str(&format!("  %{} = or %{}, %{}\n", CNT + 3, CNT + 1, CNT + 2));
                CNT += 3;
                Ok(CNT)
            },
        }
    }
}
