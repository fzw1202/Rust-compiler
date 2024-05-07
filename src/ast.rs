use std::{collections::HashMap, io};

#[derive(Clone)]
pub enum Symbol {
    Const(i32),
    Var(String),
}

static mut CNT: i32 = 0;
static mut VAR_CNT: i32 = 0;
static mut RET_CNT: i32 = 0;

pub struct Scopes {
    pub symbols: Vec<HashMap<String, Symbol>>,
}

impl Scopes {
    pub fn new() -> Self {
       Self {
            symbols: vec![],
       }
    }
}

pub struct CompUnit {
    pub func_def: FuncDef,
}

impl CompUnit {
    pub fn ir(&self, s: &mut String, scope: &mut Scopes) -> io::Result<()> {
        self.func_def.ir(s, scope)?;
        Ok(())
    }
}

pub struct FuncDef {
    pub func_type: FuncType,
    pub ident: String,
    pub block: Block,
}

impl FuncDef {
    fn ir(&self, s: &mut String, scope: &mut Scopes) -> io::Result<()> {
        s.push_str(&format!("fun @{}(): {} {{\n", self.ident, self.func_type.ir()));
        s.push_str("%entry:\n");
        self.block.ir(s, scope)?;
        s.push_str("}\n");
        Ok(())
    }
}

pub enum FuncType {
    Int,
}

impl FuncType {
    fn ir(&self) -> String {
        match self {
            FuncType::Int => "i32".to_string(),
        }
    }
}

pub struct Block {
    pub bitems: Vec<BlockItem>,
}

impl Block {
    fn ir(&self, s: &mut String, scope: &mut Scopes) -> io::Result<()> {
        unsafe {
            if RET_CNT > 0 {
                return Ok(());
            }
        }

        scope.symbols.push(HashMap::new());
        for bitem in &self.bitems {
            bitem.ir(s, scope)?;
            match bitem {
                BlockItem::Stm(stmt) => {
                    match stmt {
                        Stmt::Ret(_exp) => unsafe {
                            RET_CNT += 1;
                            break
                        },
                        _ => (),
                    }
                }
                _ => (),
            }
        }
        scope.symbols.pop();
        Ok(())
    }
}

pub enum BlockItem {
    Dec(Decl),
    Stm(Stmt),
}

impl BlockItem {
    fn ir(&self, s: &mut String, scope: &mut Scopes) -> io::Result<()> {
        match self {
            BlockItem::Dec(decl) => decl.ir(s, scope)?,
            BlockItem::Stm(stmt) => stmt.ir(s, scope)?,
        };
        Ok(())
    }
}

pub enum Decl {
    Const(ConstDecl),
    Var(VarDecl),
}

impl Decl {
    fn symbol(&self, scope: &mut Scopes) -> io::Result<()> {
        match self {
            Decl::Const(cdecl) => cdecl.symbol(scope)?,
            Decl::Var(vdecl) => vdecl.symbol(scope)?,
        };
        Ok(())
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> io::Result<()> {
        self.symbol(scope)?;
        match self {
            Decl::Const(_cdecl) => (),
            Decl::Var(vdecl) => vdecl.ir(s, scope)?,
        };
        Ok(())
    }
}

pub struct ConstDecl {
    pub btype: BType,
    pub cdefs: Vec<ConstDef>,
}

impl ConstDecl {
    fn symbol(&self, scope: &mut Scopes) -> io::Result<()> {
        match self.btype {
            BType::INT => {
                for cdef in &self.cdefs {
                    cdef.symbol(scope)?;
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
    fn symbol(&self, scope: &mut Scopes) -> io::Result<()> {
        match self.btype {
            BType::INT => {
                for vdef in &self.vdefs {
                    vdef.symbol(scope)?;
                }
            }
        };
        Ok(())
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> io::Result<()> {
        match self.btype {
            BType::INT => {
                for vdef in &self.vdefs {
                    vdef.ir(s, &"i32".to_string(), scope)?;
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
    fn symbol(&self, scope: &mut Scopes) -> io::Result<()> {
        let value: i32 = self.cinitval.symbol(scope);
        let len = scope.symbols.len() - 1;
        scope.symbols[len].insert(self.ident.clone(), Symbol::Const(value));
        Ok(())
    }
}

pub enum VarDef {
    Def(String),
    Ass(String, InitVal),
}

impl VarDef {
    fn symbol(&self, scope: &mut Scopes) -> io::Result<()> {
        let len = scope.symbols.len() - 1;
        match self {
            VarDef::Def(name) => unsafe {
                scope.symbols[len].insert(name.to_string(), Symbol::Var(format!("@{}_{}", name, VAR_CNT)));
                VAR_CNT += 1;
            },
            VarDef::Ass(name, _initval) => unsafe {
                scope.symbols[len].insert(name.to_string(), Symbol::Var(format!("@{}_{}",name, VAR_CNT)));
                VAR_CNT += 1;
            },
        };
        Ok(())
    }

    fn ir(&self, s: &mut String, t: &String, scope: &mut Scopes) -> io::Result<()> {
        let len = scope.symbols.len() - 1;
        match self {
            VarDef::Def(name) => {
                let varname = scope.symbols[len].get(name).unwrap();
                match varname {
                    Symbol::Var(n) => s.push_str(&format!("  {} = alloc {}\n", &n, t)),
                    _ => (),
                };
            },
            VarDef::Ass(name, initval) => {
                let varname = scope.symbols[len].get(name).unwrap().clone();
                match varname {
                    Symbol::Var(n) => {
                        let result = initval.ir(s, scope);
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
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        self.cexp.symbol(scope)
    }
}

pub struct InitVal {
    pub exp: Exp,
}

impl InitVal {
    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        self.exp.ir(s, scope)
    }
}
pub struct ConstExp {
    pub exp: Exp,
}

impl ConstExp {
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        self.exp.symbol(scope)
    }
}

pub struct LVal {
    pub ident: String,
}

impl LVal {
    fn get_value(&self, scope: &mut Scopes) -> Symbol {
        let mut len = scope.symbols.len() - 1;
        while len as i32 >= 0 {
            if let Some(value) = scope.symbols[len].get(&self.ident) {
                return value.clone();
            }
            len -= 1;
        }
        panic!("can not find symbol: {}", &self.ident);
    }
}

pub enum Stmt {
    Ass(LVal, Exp),
    Exp(Option<Exp>),
    Blk(Block),
    Ret(Exp),
}

impl Stmt {
    fn ir(&self, s: &mut String, scope: &mut Scopes) -> io::Result<()> {
        match self {
            Stmt::Ass(lval, exp) => match lval.get_value(scope) {
                Symbol::Const(_value) => {
                    panic!("assignment for const value is not allowed!")
                }
                Symbol::Var(name) => {
                    let result = exp.ir(s, scope);
                    match result {
                        Ok(cnt) => s.push_str(&format!("  store %{}, {}\n", cnt, name)),
                        Err(value) => s.push_str(&format!("  store {}, {}\n", value, name)),
                    }
                }
            },
            Stmt::Exp(oexp) => {
                match oexp {
                    Some(exp) => 
                        _ = exp.ir(s, scope),
                    None => (),
                }
            },
            Stmt::Blk(block) => {
                block.ir(s, scope)?;
            },
            Stmt::Ret(exp) => unsafe {
                if RET_CNT > 0 {
                    return Ok(());
                }
                
                let result = exp.ir(s, scope);
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
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        self.loexp.symbol(scope)
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        self.loexp.ir(s, scope)
    }
}

pub enum PrimaryExp {
    Exp(Box<Exp>),
    Number(i32),
    LVal(Box<LVal>),
}

impl PrimaryExp {
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            PrimaryExp::Exp(exp) => exp.symbol(scope),
            PrimaryExp::Number(ref num) => *num,
            PrimaryExp::LVal(lval) => match lval.get_value(scope) {
                Symbol::Const(value) => value,
                Symbol::Var(_name) => {
                    panic!("can not use variables to assign const value!")
                }
            },
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            PrimaryExp::Exp(exp) => exp.ir(s, scope),
            PrimaryExp::Number(num) => Err(*num),
            PrimaryExp::LVal(lval) => unsafe {
                match lval.get_value(scope) {
                    Symbol::Const(value) => Err(value),
                    Symbol::Var(name) => {
                        s.push_str(&format!("  %{} = load {}\n", CNT, name));
                        CNT += 1;
                        Ok(CNT - 1)
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
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            UnaryExp::Primary(pexp) => pexp.symbol(scope),
            UnaryExp::Unary(uop, uexp) => {
                let a = uexp.symbol(scope);
                match uop {
                    UnaryOp::Pos => a,
                    UnaryOp::Neg => -a,
                    UnaryOp::Not => (a == 0) as i32,
                }
            }
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            UnaryExp::Primary(pexp) => pexp.ir(s, scope),
            UnaryExp::Unary(uop, uexp) => unsafe {
                let result = uexp.ir(s, scope);
                match uop {
                    UnaryOp::Pos => result,
                    UnaryOp::Neg => {
                        match result {
                            Ok(cnt) => s.push_str(&format!("  %{} = sub 0, %{}\n", CNT, cnt)),
                            Err(value) => s.push_str(&format!("  %{} = sub 0, {}\n", CNT, value)),
                        };
                        CNT += 1;
                        Ok(CNT - 1)
                    }
                    UnaryOp::Not => {
                        match result {
                            Ok(cnt) => s.push_str(&format!("  %{} = eq %{}, 0\n", CNT, cnt)),
                            Err(value) => s.push_str(&format!("  %{} = eq {}, 0\n", CNT, value)),
                        };
                        CNT += 1;
                        Ok(CNT - 1)
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
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            MulExp::Unary(uexp) => uexp.symbol(scope),
            MulExp::Mul(mexp, mop, uexp) => {
                let a: i32 = mexp.symbol(scope);
                let b: i32 = uexp.symbol(scope);
                match mop {
                    MulOp::Div => a / b,
                    MulOp::Mod => a % b,
                    MulOp::Mul => a * b,
                }
            }
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            MulExp::Unary(uexp) => uexp.ir(s, scope),
            MulExp::Mul(mexp, mop, uexp) => unsafe {
                let l = mexp.ir(s, scope);
                let r = uexp.ir(s, scope);
                match l {
                    Ok(lcnt) => {
                        match r {
                            Ok(rcnt) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!("  %{} = div %{}, %{}\n", CNT, lcnt, rcnt)),
                                    MulOp::Mod => s.push_str(&format!("  %{} = mod %{}, %{}\n", CNT, lcnt, rcnt)),
                                    MulOp::Mul => s.push_str(&format!("  %{} = mul %{}, %{}\n", CNT, lcnt, rcnt)),
                                };
                            }
                            Err(rvalue) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!("  %{} = div %{}, {}\n", CNT, lcnt, rvalue)),
                                    MulOp::Mod => s.push_str(&format!("  %{} = mod %{}, {}\n", CNT, lcnt, rvalue)),
                                    MulOp::Mul => s.push_str(&format!("  %{} = mul %{}, {}\n", CNT, lcnt, rvalue)),
                                };
                            }
                        };
                    }
                    Err(lvalue) => {
                        match r {
                            Ok(rcnt) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!("  %{} = div {}, %{}\n", CNT, lvalue, rcnt)),
                                    MulOp::Mod => s.push_str(&format!("  %{} = mod {}, %{}\n", CNT, lvalue, rcnt)),
                                    MulOp::Mul => s.push_str(&format!("  %{} = mul {}, %{}\n", CNT, lvalue, rcnt)),
                                };
                            }
                            Err(rvalue) => {
                                match mop {
                                    MulOp::Div => s.push_str(&format!("  %{} = div {}, {}\n", CNT, lvalue, rvalue)),
                                    MulOp::Mod => s.push_str(&format!("  %{} = mod {}, {}\n", CNT, lvalue, rvalue)),
                                    MulOp::Mul => s.push_str(&format!("  %{} = mul {}, {}\n", CNT, lvalue, rvalue)),
                                };
                            }
                        };
                    }
                };
                CNT += 1;
                Ok(CNT - 1)
            },
        }
    }
}

pub enum AddExp {
    Mul(MulExp),
    Add(Box<AddExp>, AddOp, MulExp),
}

impl AddExp {
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            AddExp::Mul(mexp) => mexp.symbol(scope),
            AddExp::Add(aexp, aop, mexp) => {
                let a: i32 = aexp.symbol(scope);
                let b: i32 = mexp.symbol(scope);
                match aop {
                    AddOp::Add => a + b,
                    AddOp::Sub => a - b,
                }
            }
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            AddExp::Mul(mexp) => mexp.ir(s, scope),
            AddExp::Add(aexp, aop, mexp) => unsafe {
                let l = aexp.ir(s, scope);
                let r = mexp.ir(s, scope);
                match l {
                    Ok(lcnt) => {
                        match r {
                            Ok(rcnt) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!("  %{} = add %{}, %{}\n", CNT, lcnt, rcnt)),
                                    AddOp::Sub => s.push_str(&format!("  %{} = sub %{}, %{}\n", CNT, lcnt, rcnt)),
                                };
                            }
                            Err(rvalue) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!("  %{} = add %{}, {}\n", CNT, lcnt, rvalue)),
                                    AddOp::Sub => s.push_str(&format!("  %{} = sub %{}, {}\n", CNT, lcnt, rvalue)),
                                };
                            }
                        };
                    }
                    Err(lvalue) => {
                        match r {
                            Ok(rcnt) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!("  %{} = add {}, %{}\n", CNT, lvalue, rcnt)),
                                    AddOp::Sub => s.push_str(&format!("  %{} = sub {}, %{}\n", CNT, lvalue, rcnt)),
                                };
                            }
                            Err(rvalue) => {
                                match aop {
                                    AddOp::Add => s.push_str(&format!("  %{} = add {}, {}\n", CNT, lvalue, rvalue)),
                                    AddOp::Sub => s.push_str(&format!("  %{} = sub {}, {}\n", CNT, lvalue, rvalue)),
                                };
                            }
                        };
                    }
                };
                CNT += 1;
                Ok(CNT - 1)
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
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            RelExp::Add(aexp) => aexp.symbol(scope),
            RelExp::Rel(rexp, rop, aexp) => {
                let a: i32 = rexp.symbol(scope);
                let b: i32 = aexp.symbol(scope);
                match rop {
                    RelOp::Less => (a < b) as i32,
                    RelOp::Greater => (a > b) as i32,
                    RelOp::LessEq => (a <= b) as i32,
                    RelOp::GreaterEq => (a >= b) as i32,
                }
            }
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            RelExp::Add(aexp) => aexp.ir(s, scope),
            RelExp::Rel(rexp, rop, aexp) => unsafe {
                let l = rexp.ir(s, scope);
                let r = aexp.ir(s, scope);
                match l {
                    Ok(lcnt) => match r {
                        Ok(rcnt) => {
                            match rop {
                                RelOp::Less => s.push_str(&format!("  %{} = lt %{}, %{}\n", CNT, lcnt, rcnt)),
                                RelOp::Greater => s.push_str(&format!("  %{} = gt %{}, %{}\n", CNT, lcnt, rcnt)),
                                RelOp::LessEq => s.push_str(&format!("  %{} = le %{}, %{}\n", CNT, lcnt, rcnt)),
                                RelOp::GreaterEq => s.push_str(&format!("  %{} = ge %{}, %{}\n", CNT, lcnt, rcnt)),
                            };
                        }
                        Err(rvalue) => {
                            match rop {
                                RelOp::Less => s.push_str(&format!("  %{} = lt %{}, {}\n", CNT, lcnt, rvalue)),
                                RelOp::Greater => s.push_str(&format!("  %{} = gt %{}, {}\n", CNT, lcnt, rvalue)),
                                RelOp::LessEq => s.push_str(&format!("  %{} = le %{}, {}\n", CNT, lcnt, rvalue)),
                                RelOp::GreaterEq => s.push_str(&format!("  %{} = ge %{}, {}\n", CNT, lcnt, rvalue)),
                            };
                        }
                    },
                    Err(lvalue) => match r {
                        Ok(rcnt) => {
                            match rop {
                                RelOp::Less => s.push_str(&format!("  %{} = lt {}, %{}\n", CNT, lvalue, rcnt)),
                                RelOp::Greater => s.push_str(&format!("  %{} = gt {}, %{}\n", CNT, lvalue, rcnt)),
                                RelOp::LessEq => s.push_str(&format!("  %{} = le {}, %{}\n", CNT, lvalue, rcnt)),
                                RelOp::GreaterEq => s.push_str(&format!("  %{} = ge {}, %{}\n", CNT, lvalue, rcnt)),
                            };
                        }
                        Err(rvalue) => {
                            match rop {
                                RelOp::Less => s.push_str(&format!("  %{} = lt {}, {}\n", CNT, lvalue, rvalue)),
                                RelOp::Greater => s.push_str(&format!("  %{} = gt {}, {}\n", CNT, lvalue, rvalue)),
                                RelOp::LessEq => s.push_str(&format!("  %{} = le {}, {}\n", CNT, lvalue, rvalue)),
                                RelOp::GreaterEq => s.push_str(&format!("  %{} = ge {}, {}\n", CNT, lvalue, rvalue)),
                            };
                        }
                    },
                }
                CNT += 1;
                Ok(CNT - 1)
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
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            EqExp::Rel(rexp) => rexp.symbol(scope),
            EqExp::Eq(eexp, eop, rexp) => {
                let a: i32 = eexp.symbol(scope);
                let b: i32 = rexp.symbol(scope);
                match eop {
                    EqOp::Eq => (a == b) as i32,
                    EqOp::Neq => (a != b) as i32,
                }
            }
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            EqExp::Rel(rexp) => rexp.ir(s, scope),
            EqExp::Eq(eexp, eop, rexp) => unsafe {
                let l = eexp.ir(s, scope);
                let r = rexp.ir(s, scope);
                match l {
                    Ok(lcnt) => match r {
                        Ok(rcnt) => match eop {
                            EqOp::Eq => s.push_str(&format!("  %{} = eq %{}, %{}\n", CNT, lcnt, rcnt)),
                            EqOp::Neq => s.push_str(&format!("  %{} = ne %{}, %{}\n", CNT, lcnt, rcnt)),
                        },
                        Err(rvalue) => match eop {
                            EqOp::Eq => s.push_str(&format!("  %{} = eq %{}, {}\n", CNT, lcnt, rvalue)),
                            EqOp::Neq => s.push_str(&format!("  %{} = ne %{}, {}\n", CNT, lcnt, rvalue)),
                        },
                    },
                    Err(lvalue) => match r {
                        Ok(rcnt) => match eop {
                            EqOp::Eq => s.push_str(&format!("  %{} = eq {}, %{}\n", CNT, lvalue, rcnt)),
                            EqOp::Neq => s.push_str(&format!("  %{} = ne {}, %{}\n", CNT, lvalue, rcnt)),
                        },
                        Err(rvalue) => match eop {
                            EqOp::Eq => s.push_str(&format!("  %{} = eq {}, {}\n", CNT, lvalue, rvalue)),
                            EqOp::Neq => s.push_str(&format!("  %{} = ne {}, {}\n", CNT, lvalue, rvalue)),
                        },
                    },
                }
                CNT += 1;
                Ok(CNT - 1)
            },
        }
    }
}

pub enum LAndExp {
    Eq(EqExp),
    LAnd(Box<LAndExp>, EqExp),
}

impl LAndExp {
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            LAndExp::Eq(eexp) => eexp.symbol(scope),
            LAndExp::LAnd(laexp, eexp) => {
                let a: i32 = laexp.symbol(scope);
                let b: i32 = eexp.symbol(scope);
                (a != 0 && b != 0) as i32
            }
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            LAndExp::Eq(eexp) => eexp.ir(s, scope),
            LAndExp::LAnd(laexp, eexp) => unsafe {
                let l = laexp.ir(s, scope);
                let r = eexp.ir(s, scope);
                match l {
                    Ok(lcnt) => s.push_str(&format!("  %{} = eq %{}, 0\n", CNT, lcnt)),
                    Err(lvalue) => s.push_str(&format!("  %{} = eq {}, 0\n", CNT, lvalue)),
                };
                match r {
                    Ok(rcnt) => s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 1, rcnt)),
                    Err(rvalue) => s.push_str(&format!("  %{} = eq {}, 0\n", CNT + 1, rvalue)),
                };
                s.push_str(&format!("  %{} = or %{}, %{}\n", CNT + 2, CNT, CNT + 1));
                s.push_str(&format!("  %{} = eq %{}, 0\n", CNT + 3, CNT + 2));
                CNT += 4;
                Ok(CNT - 1)
            },
        }
    }
}

pub enum LOrExp {
    LAnd(LAndExp),
    LOr(Box<LOrExp>, LAndExp),
}

impl LOrExp {
    fn symbol(&self, scope: &mut Scopes) -> i32 {
        match self {
            LOrExp::LAnd(laexp) => laexp.symbol(scope),
            LOrExp::LOr(loexp, laexp) => {
                let a: i32 = loexp.symbol(scope);
                let b: i32 = laexp.symbol(scope);
                (a != 0 || b != 0) as i32
            }
        }
    }

    fn ir(&self, s: &mut String, scope: &mut Scopes) -> Result<i32, i32> {
        match self {
            LOrExp::LAnd(laexp) => laexp.ir(s, scope),
            LOrExp::LOr(loexp, laexp) => unsafe {
                let l = loexp.ir(s, scope);
                let r = laexp.ir(s, scope);
                match l {
                    Ok(lcnt) => s.push_str(&format!("  %{} = ne %{}, 0\n", CNT, lcnt)),
                    Err(lvalue) => s.push_str(&format!("  %{} = ne {}, 0\n", CNT, lvalue)),
                };
                match r {
                    Ok(rcnt) => s.push_str(&format!("  %{} = ne %{}, 0\n", CNT + 1, rcnt)),
                    Err(rvalue) => s.push_str(&format!("  %{} = ne {}, 0\n", CNT + 1, rvalue)),
                };
                s.push_str(&format!("  %{} = or %{}, %{}\n", CNT + 2, CNT, CNT + 1));
                CNT += 3;
                Ok(CNT - 1)
            },
        }
    }
}
