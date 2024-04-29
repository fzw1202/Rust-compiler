use koopa::ir::entities::Value;
use koopa::ir::BinaryOp;
use koopa::ir::FunctionData;
use koopa::ir::ValueKind;
use std::collections::HashMap;

pub struct StackFrame {
    size: i32,
    pos: HashMap<Value, i32>,
}

impl StackFrame {
    fn new() -> Self {
        let stack_frame = StackFrame {
            size: 0,
            pos: HashMap::new(),
        };
        stack_frame
    }
}

pub trait GenerateAsm {
    fn generate(&self, _func_data: Option<&FunctionData>, _frame: Option<&StackFrame>) -> String;
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, _func_data: Option<&FunctionData>, _frame: Option<&StackFrame>) -> String {
        let mut s: String = String::new();
        s.push_str("  .text\n");
        s.push_str("  .globl main\n");
        for &func in self.func_layout() {
            s.push_str(&self.func(func).generate(None, None));
        }
        s
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, _func_data: Option<&FunctionData>, _frame: Option<&StackFrame>) -> String {
        let mut s: String = String::new();
        s.push_str(&format!("{}:\n", &self.name()[1..]));

        let mut frame = StackFrame::new();
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let data = self.dfg().value(inst);
                if !data.ty().is_unit() {
                    frame.pos.insert(inst, frame.size);
                    frame.size += 4;
                }
            }
        }

        frame.size = if frame.size % 16 != 0 {
            frame.size + 16 - (frame.size % 16)
        } else {
            frame.size
        };

        if frame.size > 2048 {
            s.push_str(&format!("  li t0, {}\n", -frame.size));
            s.push_str(&format!("  add sp, sp, t0\n"));

        } else {
            s.push_str(&format!("  addi sp, sp, -{}\n", frame.size));
        }

        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                s.push_str(&inst.generate(Some(self), Some(&frame)));
            }
        }
        s
    }
}

impl GenerateAsm for Value {
    fn generate(&self, func_data: Option<&FunctionData>, frame: Option<&StackFrame>) -> String {
        let data = func_data.unwrap().dfg().value(*self);
        let mut s = String::new();

        match data.kind() {
            ValueKind::Integer(int) => int.value().to_string(),
            ValueKind::Return(ret) => {
                s.push_str(&format!(
                    "  lw a0, {}(sp)\n",
                    frame.unwrap().pos.get(&ret.value().unwrap()).unwrap()
                ));
                if frame.unwrap().size > 2047 {
                    s.push_str(&format!("  li t0, {}\n", frame.unwrap().size));
                    s.push_str(&format!("  add sp, sp, t0\n"));
        
                } else {
                    s.push_str(&format!("  addi sp, sp, {}\n", frame.unwrap().size));
                }
                s.push_str("  ret\n");
                s
            }
            ValueKind::Alloc(_alloc) => s,
            ValueKind::Store(store) => {
                let src = func_data.unwrap().dfg().value(store.value());
                match src.kind() {
                    ValueKind::Integer(int) => {
                        s.push_str(&format!("  li t0, {}\n", int.value()));
                    }
                    _ => {
                        let offset = frame.unwrap().pos.get(&store.value()).unwrap();
                        s.push_str(&format!("  lw t0, {}(sp)\n", offset));
                    }
                }
                let offset = *frame.unwrap().pos.get(&store.dest()).unwrap();
                s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                s
            }
            ValueKind::Load(load) => {
                let des = *frame.unwrap().pos.get(self).unwrap();
                let src = *frame.unwrap().pos.get(&load.src()).unwrap();
                s.push_str(&format!("  lw t0, {}(sp)\n", src));
                s.push_str(&format!("  sw t0, {}(sp)\n", des));
                s
            }
            ValueKind::Binary(bin) => {
                let op = bin.op();
                let offset = *frame.unwrap().pos.get(self).unwrap();
                let lhs = func_data.unwrap().dfg().value(bin.lhs());
                let rhs = func_data.unwrap().dfg().value(bin.rhs());

                let l = match lhs.kind() {
                    ValueKind::Integer(int) => {
                        if int.value() == 0 {
                            "x0".to_string()
                        } else {
                            s.push_str(&format!("  li t0, {}\n", int.value()));
                            "t0".to_string()
                        }
                    }
                    _ => {
                        let loff = *frame.unwrap().pos.get(&bin.lhs()).unwrap();
                        s.push_str(&format!("  lw t0, {}(sp)\n", loff));
                        "t0".to_string()
                    }
                };

                let r = match rhs.kind() {
                    ValueKind::Integer(int) => {
                        if int.value() == 0 {
                            "x0".to_string()
                        } else {
                            s.push_str(&format!("  li t1, {}\n", int.value()));
                            "t1".to_string()
                        }
                    }
                    _ => {
                        let roff = *frame.unwrap().pos.get(&bin.rhs()).unwrap();
                        s.push_str(&format!("  lw t1, {}(sp)\n", roff));
                        "t1".to_string()
                    }
                };

                match op {
                    BinaryOp::NotEq => {
                        s.push_str(&format!("  xor t0, {}, {}\n", l, r));
                        s.push_str(&format!("  snez t0, t0\n"));
                    }
                    BinaryOp::Eq => {
                        s.push_str(&format!("  xor t0, {}, {}\n", l, r));
                        s.push_str(&format!("  seqz t0, t0\n"));
                    }
                    BinaryOp::Gt => {
                        s.push_str(&format!("  sgt t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Lt => {
                        s.push_str(&format!("  slt t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Ge => {
                        s.push_str(&format!("  slt t0, {}, {}\n", l, r));
                        s.push_str(&format!("  seqz t0, t0\n"));
                    }
                    BinaryOp::Le => {
                        s.push_str(&format!("  sgt t0, {}, {}\n", l, r));
                        s.push_str(&format!("  seqz t0, t0\n"));
                    }
                    BinaryOp::Add => {
                        s.push_str(&format!("  add t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Sub => {
                        if l == "x0" && r == "x0" {
                            s.push_str(&format!("  sw x0, {}(sp)\n", offset));
                        } else {
                            s.push_str(&format!("  sub t0, {}, {}\n", l, r));
                            s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                        }
                    }
                    BinaryOp::Mul => {
                        s.push_str(&format!("  mul t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Div => {
                        s.push_str(&format!("  div t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Mod => {
                        s.push_str(&format!("  rem t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::And => {
                        s.push_str(&format!("  and t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Or => {
                        s.push_str(&format!("  or t0, {}, {}\n", l, r));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    _ => {}
                }
                s
            }
            _ => unreachable!(),
        }
    }
}
