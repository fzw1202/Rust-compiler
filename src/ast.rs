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
        }
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
        }
        Ok(())
    }

    fn dump(&self, s: &mut String) -> io::Result<()> {
        match self.btype {
            BType::INT => {
                for vdef in &self.vdefs {
                    vdef.dump(s, &"i32".to_string())?;
                }
            }
        }
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
                        let a = initval.dump(s);
                        s.push_str(&format!("  {} = alloc {}\n", &n, t));
                        s.push_str(&format!("  store %{}, {}\n", a, &n));
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
    fn dump(&self, s: &mut String) -> i32 {
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
                Symbol::Const(_value) => panic!("assignment for const value is not allowed!"),
                Symbol::Var(name) => {
                    let a = exp.dump(s);
                    s.push_str(&format!("  store %{}, {}\n", a, name));
                }
            },
            Stmt::Ret(exp) => {
                let a = exp.dump(s);
                s.push_str(&format!("  ret %{}\n", a));
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

    fn dump(&self, s: &mut String) -> i32 {
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
                Symbol::Var(_name) => panic!("can not use variables to assign const value!"),
            },
        }
    }

    fn dump(&self, s: &mut String) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.dump(s),
            PrimaryExp::Number(ref num) => unsafe {
                CNT += 1;
                s.push_str(&format!("  %{} = sub {}, 0\n", CNT, num));
                return CNT;
            },
            PrimaryExp::LVal(lval) => unsafe {
                CNT += 1;
                match lval.get_value() {
                    Symbol::Const(value) => s.push_str(&format!("  %{} = sub {}, 0\n", CNT, value)),
                    Symbol::Var(name) => s.push_str(&format!("  %{} = load {}\n", CNT, name)),
                };
                return CNT;
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
                    UnaryOp::Not => (a != 0) as i32,
                }
            }
        }
    }

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
                        }
                        UnaryOp::Not => {
                            CNT += 1;
                            s.push_str(&format!("  %{} = eq %{}, 0\n", CNT, a));
                            return CNT;
                        }
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

    fn dump(&self, s: &mut String) -> i32 {
        match self {
            MulExp::Unary(uexp) => uexp.dump(s),
            MulExp::Mul(mexp, mop, uexp) => {
                let a: i32 = mexp.dump(s);
                let b: i32 = uexp.dump(s);
                unsafe {
                    CNT += 1;
                    match mop {
                        MulOp::Div => s.push_str(&format!("  %{} = div %{}, %{}\n", CNT, a, b)),
                        MulOp::Mod => s.push_str(&format!("  %{} = mod %{}, %{}\n", CNT, a, b)),
                        MulOp::Mul => s.push_str(&format!("  %{} = mul %{}, %{}\n", CNT, a, b)),
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

    fn dump(&self, s: &mut String) -> i32 {
        match self {
            AddExp::Mul(mexp) => mexp.dump(s),
            AddExp::Add(aexp, aop, mexp) => {
                let a: i32 = aexp.dump(s);
                let b: i32 = mexp.dump(s);
                unsafe {
                    CNT += 1;
                    match aop {
                        AddOp::Add => s.push_str(&format!("  %{} = add %{}, %{}\n", CNT, a, b)),
                        AddOp::Sub => s.push_str(&format!("  %{} = sub %{}, %{}\n", CNT, a, b)),
                    };
                    return CNT;
                }
            }
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

    fn dump(&self, s: &mut String) -> i32 {
        match self {
            RelExp::Add(aexp) => aexp.dump(s),
            RelExp::Rel(rexp, rop, aexp) => {
                let a: i32 = rexp.dump(s);
                let b: i32 = aexp.dump(s);
                unsafe {
                    CNT += 1;
                    match rop {
                        RelOp::Less => s.push_str(&format!("  %{} = lt %{}, %{}\n", CNT, a, b)),
                        RelOp::Greater => s.push_str(&format!("  %{} = gt %{}, %{}\n", CNT, a, b)),
                        RelOp::LessEq => s.push_str(&format!("  %{} = le %{}, %{}\n", CNT, a, b)),
                        RelOp::GreaterEq => {
                            s.push_str(&format!("  %{} = ge %{}, %{}\n", CNT, a, b))
                        }
                    };
                    return CNT;
                }
            }
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

    fn dump(&self, s: &mut String) -> i32 {
        match self {
            EqExp::Rel(rexp) => rexp.dump(s),
            EqExp::Eq(eexp, eop, rexp) => {
                let a: i32 = eexp.dump(s);
                let b: i32 = rexp.dump(s);
                unsafe {
                    CNT += 1;
                    match eop {
                        EqOp::Eq => s.push_str(&format!("  %{} = eq %{}, %{}\n", CNT, a, b)),
                        EqOp::Neq => s.push_str(&format!("  %{} = ne %{}, %{}\n", CNT, a, b)),
                    }
                    return CNT;
                }
            }
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

    fn dump(&self, s: &mut String) -> i32 {
        match self {
            LAndExp::Eq(eexp) => eexp.dump(s),
            LAndExp::LAnd(laexp, eexp) => {
                let a: i32 = laexp.dump(s);
                let b: i32 = eexp.dump(s);
                unsafe {
                    s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 1, a));
                    s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 2, b));
                    s.push_str(&format!("  %{} = or %{}, %{}\n", CNT + 3, CNT + 1, CNT + 2));
                    s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 4, CNT + 3));
                    CNT += 4;
                    return CNT;
                }
            }
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

    fn dump(&self, s: &mut String) -> i32 {
        match self {
            LOrExp::LAnd(laexp) => laexp.dump(s),
            LOrExp::LOr(loexp, laexp) => {
                let a: i32 = loexp.dump(s);
                let b: i32 = laexp.dump(s);
                unsafe {
                    s.push_str(&format!("  %{} = ne %{}, 0\n", CNT + 1, a));
                    s.push_str(&format!("  %{} = ne %{}, 0\n", CNT + 2, b));
                    s.push_str(&format!("  %{} = or %{}, %{}\n", CNT + 3, CNT + 1, CNT + 2));
                    CNT += 3;
                    return CNT;
                }
            }
        }
    }
}
