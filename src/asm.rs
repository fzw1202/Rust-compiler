use koopa::ir::BinaryOp;
use koopa::ir::FunctionData;
use koopa::ir::ValueKind;
use once_cell::sync::Lazy;
use std::collections::HashMap;
use std::collections::VecDeque;

static mut REG_NAME: [&str; 15] = [
    "t0", "t1", "t2", "t3", "t4", "t5", "t6", "a0", "a1", "a2", "a3", "a4", "a5", "a6", "a7",
];

static mut REG: Lazy<VecDeque<usize>> = Lazy::new(|| VecDeque::new());
static mut REG_RECORD: Lazy<HashMap<koopa::ir::entities::Value, usize>> =
    Lazy::new(|| HashMap::new());

pub fn init() {
    unsafe {
        for i in 0..REG_NAME.len() {
            REG.push_back(i);
        }
    }
}

pub trait GenerateAsm {
    fn generate(&self, _func_data: Option<&FunctionData>) -> String;
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, _func_data: Option<&FunctionData>) -> String {
        let mut s: String = String::new();
        s.push_str("  .text\n");
        s.push_str("  .globl main\n");
        for &func in self.func_layout() {
            s.push_str(&self.func(func).generate(None));
        }
        s
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, _func_data: Option<&FunctionData>) -> String {
        let mut s: String = String::new();
        s.push_str(&format!("{}:\n", &self.name()[1..]));
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                s.push_str(&inst.generate(Some(self)));
            }
        }
        s
    }
}

impl GenerateAsm for koopa::ir::entities::Value {
    fn generate(&self, func_data: Option<&FunctionData>) -> String {
        let data = func_data.unwrap().dfg().value(*self);

        match data.kind() {
            ValueKind::Integer(int) => int.value().to_string(),
            ValueKind::Return(ret) => {
                let mut s = String::new();
                unsafe {
                    s.push_str(&format!(
                        "  mv a0, {}\n  ret\n",
                        REG_NAME[REG_RECORD[&ret.value().unwrap()]]
                    ));
                }
                s
            }
            ValueKind::Binary(bin) => {
                let mut s = String::new();
                let op = bin.op();
                let lhs = func_data.unwrap().dfg().value(bin.lhs());
                let rhs = func_data.unwrap().dfg().value(bin.rhs());

                let mut regl: usize = usize::MAX;
                let l = match lhs.kind() {
                    ValueKind::Integer(int) => unsafe {
                        if int.value() == 0 {
                            "x0".to_string()
                        } else {
                            regl = REG.pop_front().unwrap();
                            let reg = REG_NAME[regl];
                            regl += REG_NAME.len();
                            s.push_str(&format!("  li {}, {}\n", reg, int.value()));
                            reg.to_string()
                        }
                    },
                    _ => unsafe {
                        regl = REG_RECORD[&bin.lhs()];
                        REG_NAME[regl].to_string()
                    },
                };

                let mut regr: usize = usize::MAX;
                let r = match rhs.kind() {
                    ValueKind::Integer(int) => unsafe {
                        if int.value() == 0 {
                            "x0".to_string()
                        } else {
                            let reg = REG_NAME[REG.pop_front().unwrap()];
                            s.push_str(&format!("  li {}, {}\n", reg, int.value()));
                            reg.to_string()
                        }
                    },
                    _ => unsafe {
                        regr = REG_RECORD[&bin.rhs()];
                        REG_NAME[regr].to_string()
                    },
                };

                unsafe {
                    let reg = REG.pop_front().unwrap();
                    match op {
                        BinaryOp::NotEq => {
                            s.push_str(&format!("  xor {}, {}, {}\n", REG_NAME[reg], l, r));
                            s.push_str(&format!("  snez {}, {}\n", REG_NAME[reg], REG_NAME[reg]));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Eq => {
                            s.push_str(&format!("  xor {}, {}, {}\n", REG_NAME[reg], l, r));
                            s.push_str(&format!("  seqz {}, {}\n", REG_NAME[reg], REG_NAME[reg]));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Gt => {
                            s.push_str(&format!("  sgt {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Lt => {
                            s.push_str(&format!("  slt {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Ge => {
                            s.push_str(&format!("  slt {}, {}, {}\n", REG_NAME[reg], l, r));
                            s.push_str(&format!("  seqz {}, {}\n", REG_NAME[reg], REG_NAME[reg]));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Le => {
                            s.push_str(&format!("  sgt {}, {}, {}\n", REG_NAME[reg], l, r));
                            s.push_str(&format!("  seqz {}, {}\n", REG_NAME[reg], REG_NAME[reg]));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Add => {
                            s.push_str(&format!("  add {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Sub => {
                            if r == "x0" && l != "x0" {
                                REG.push_front(reg);
                                println!("l: {}", l);
                                REG_RECORD.insert(*self, regl % REG_NAME.len());
                            } else {
                                s.push_str(&format!("  sub {}, {}, {}\n", REG_NAME[reg], l, r));
                                REG_RECORD.insert(*self, reg);
                            }
                        }
                        BinaryOp::Mul => {
                            s.push_str(&format!("  mul {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Div => {
                            s.push_str(&format!("  div {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Mod => {
                            s.push_str(&format!("  rem {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::And => {
                            s.push_str(&format!("  and {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        BinaryOp::Or => {
                            s.push_str(&format!("  or {}, {}, {}\n", REG_NAME[reg], l, r));
                            REG_RECORD.insert(*self, reg);
                        }
                        _ => {}
                    }

                    if regl < REG_NAME.len() {
                        println!("back {}", regl);
                        REG.push_back(regl);
                        REG_RECORD.remove(&bin.lhs());
                    }

                    if regr < REG_NAME.len() {
                        println!("back {}", regr);
                        REG.push_back(regr);
                        REG_RECORD.remove(&bin.rhs());
                    }
                }
                s
            }
            _ => unreachable!(),
        }
    }
}
