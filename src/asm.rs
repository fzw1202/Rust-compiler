use koopa::ir::entities::Value;
use koopa::ir::BinaryOp;
use koopa::ir::FunctionData;
use koopa::ir::Program;
use koopa::ir::ValueKind;
use std::collections::HashMap;

pub struct StackFrame {
    call: bool,
    size: i32,
    pos: HashMap<Value, i32>,
}

impl StackFrame {
    fn new() -> Self {
        let stack_frame = StackFrame {
            call: false,
            size: 0,
            pos: HashMap::new(),
        };
        stack_frame
    }
}

pub struct Global {
    values: HashMap<Value, String>,
}

impl Global {
    fn new() -> Self {
        let global = Global {
            values: HashMap::new(),
        };
        global
    }
}

pub trait GenerateAsm {
    fn generate(&self, _func_data: Option<&FunctionData>, _frame: Option<&StackFrame>, _program: Option<&Program>, _global: Option<&Global>) -> String;
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, _func_data: Option<&FunctionData>, _frame: Option<&StackFrame>, _program: Option<&Program>, _global: Option<&Global>) -> String {
        let mut global = Global::new();
        let mut s: String = String::new();
        for &value in self.inst_layout() {
            let data = self.borrow_value(value);
            let name = &data.name().as_ref().unwrap()[1..];
            global.values.insert(value, name.to_string());
            s.push_str("  .data\n");
            s.push_str(&format!("  .globl {}\n", name));
            s.push_str(&format!("{}:\n", name));
            match data.kind() {
                ValueKind::Integer(int) => s.push_str(&format!("  .word {}\n", int.value())),
                ValueKind::ZeroInit(_) => s.push_str(&format!("  .zero 4\n\n")),
                _ => (),
            }
        }

        for &func in self.func_layout() {
            s.push_str(&self.func(func).generate(None, None, Some(self), Some(&global)));
        }
        s
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, _func_data: Option<&FunctionData>, _frame: Option<&StackFrame>, program: Option<&Program>, global: Option<&Global>) -> String {
        let global_funcs = vec![
            "getint",
            "getch",
            "getarray",
            "putint",
            "putch",
            "putarray",
            "starttime",
            "stoptime",
        ];
        if global_funcs.contains(&&self.name()[1..]) {
            return String::new();
        }

        let mut s: String = String::new();
        s.push_str("  .text\n");
        s.push_str(&format!("  .globl {}\n", &self.name()[1..]));
        s.push_str(&format!("{}:\n", &self.name()[1..]));

        let mut frame = StackFrame::new();
        let mut args = 0;

        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let data = self.dfg().value(inst);
                if let ValueKind::Call(c) = data.kind() {
                    frame.call = true;
                    if c.args().len() < 8 {
                        continue;
                    }
                    if c.args().len() - 8 > args {
                        args = c.args().len() - 8;
                    }
                }
            }
        }

        frame.size += (args * 4) as i32;

        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let data = self.dfg().value(inst);
                if !data.ty().is_unit() {
                    frame.pos.insert(inst, frame.size);
                    frame.size += 4;
                }
            }
        }

        for i in 0..self.params().len() {
            if i >= 8 {
                break;
            } else {
                frame.pos.insert(self.params()[i], frame.size);
                frame.size += 4;
            }
        }

        if frame.call {
            frame.size += 4;
        }

        frame.size = if frame.size % 16 != 0 {
            frame.size + 16 - (frame.size % 16)
        } else {
            frame.size
        };

        if frame.size > 2048 {
            s.push_str(&format!("  li t0, {}\n", -frame.size));
            s.push_str(&format!("  add sp, sp, t0\n"));
        } else if frame.size > 0 {
            s.push_str(&format!("  addi sp, sp, -{}\n", frame.size));
        }

        if frame.call {
            s.push_str(&format!("  sw ra, {}(sp)\n", frame.size - 4));
        }

        for i in 0..self.params().len() {
            if i < 8 {
                let offset = frame.pos.get(&self.params()[i]).unwrap();
                s.push_str(&format!("  sw a{}, {}(sp)\n", i, offset));
            } else {
                frame.pos.insert(self.params()[i], frame.size + ((i - 8) * 4) as i32);
            }
        }

        for (&bb, node) in self.layout().bbs() {
            let blk_name = &self.dfg().bb(bb).name().as_ref().unwrap()[1..];
            if blk_name != "entry" {
                s.push_str(&format!("\n{}:\n", blk_name));
            }
            for &inst in node.insts().keys() {
                s.push_str(&inst.generate(Some(self), Some(&frame), program, global));
            }
        }
        s
    }
}

impl GenerateAsm for Value {
    fn generate(&self, func_data: Option<&FunctionData>, frame: Option<&StackFrame>, program: Option<&Program>, _global: Option<&Global>) -> String {
        let data = func_data.unwrap().dfg().value(*self);
        let mut s = String::new();

        match data.kind() {
            ValueKind::Integer(int) => int.value().to_string(),
            ValueKind::Return(ret) => match ret.value() {
                Some(ret) => {
                    let retdata = func_data.unwrap().dfg().value(ret);
                    match retdata.kind() {
                        ValueKind::Integer(int) => {
                            s.push_str(&format!("  li a0, {}\n", int.value()));
                        }
                        _ => {
                            let offset = frame.unwrap().pos.get(&ret).unwrap();
                            s.push_str(&format!("  lw a0, {}(sp)\n", offset));
                        }
                    };

                    if frame.unwrap().call {
                        s.push_str(&format!("  lw ra, {}(sp)\n", frame.unwrap().size - 4));
                    }

                    let size = frame.unwrap().size;
                    if size > 2047 {
                        s.push_str(&format!("  li t0, {}\n", size));
                        s.push_str(&format!("  add sp, sp, t0\n"));
                    } else if size > 0 {
                        s.push_str(&format!("  addi sp, sp, {}\n", size));
                    }
                    s.push_str("  ret\n\n");
                    s
                }
                None => {
                    let size = frame.unwrap().size;
                    if size > 2047 {
                        s.push_str(&format!("  li t0, {}\n", size));
                        s.push_str(&format!("  add sp, sp, t0\n"));
                    } else if size > 0 {
                        s.push_str(&format!("  addi sp, sp, {}\n", size));
                    }
                    s.push_str("  ret\n\n");
                    s
                }
            },
            ValueKind::Alloc(_alloc) => s,
            ValueKind::Load(load) => {
                let des = *frame.unwrap().pos.get(self).unwrap();
                let src = *frame.unwrap().pos.get(&load.src()).unwrap();
                s.push_str(&format!("  lw t0, {}(sp)\n", src));
                s.push_str(&format!("  sw t0, {}(sp)\n", des));
                s
            }
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
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Eq => {
                        s.push_str(&format!("  xor t0, {}, {}\n", l, r));
                        s.push_str(&format!("  seqz t0, t0\n"));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
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
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
                    }
                    BinaryOp::Le => {
                        s.push_str(&format!("  sgt t0, {}, {}\n", l, r));
                        s.push_str(&format!("  seqz t0, t0\n"));
                        s.push_str(&format!("  sw t0, {}(sp)\n", offset));
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
            ValueKind::Branch(branch) => {
                let cond = branch.cond();
                match func_data.unwrap().dfg().value(cond).kind() {
                    ValueKind::Integer(int) => {
                        s.push_str(&format!("  li t0, {}\n", int.value()));
                    }
                    _ => {
                        let offset = frame.unwrap().pos.get(&cond).unwrap();
                        s.push_str(&format!("  lw t0, {}(sp)\n", offset));
                    }
                }
                let true_name = &func_data.unwrap().dfg().bb(branch.true_bb()).name().as_ref().unwrap()[1..];
                let false_name = &func_data.unwrap().dfg().bb(branch.false_bb()).name().as_ref().unwrap()[1..];
                s.push_str(&format!("  bnez t0, {}\n", true_name));
                s.push_str(&format!("  j {}\n", false_name));
                s
            }
            ValueKind::Jump(jump) => {
                let name = &func_data.unwrap().dfg().bb(jump.target()).name().as_ref().unwrap()[1..];
                s.push_str(&format!("  j {}\n", name));
                s
            }
            ValueKind::Call(call) => {
                for i in 0..call.args().len() {
                    if i < 8 {
                        match func_data.unwrap().dfg().value(call.args()[i]).kind() {
                            ValueKind::Integer(int) => s.push_str(&format!("  li a{}, {}\n", i, int.value())),
                            _ => {
                                let offset = frame.unwrap().pos.get(&call.args()[i]).unwrap();
                                s.push_str(&format!("  lw a{}, {}(sp)\n", i, offset))
                            }
                        }
                    } else {
                        let pos = 4 * (i - 8) as i32;
                        match func_data.unwrap().dfg().value(call.args()[i]).kind() {
                            ValueKind::Integer(int) => {
                                s.push_str(&format!("  li t0, {}\n", int.value()));
                                s.push_str(&format!("  sw t0, {}(sp)\n", pos));
                            }
                            _ => {
                                let offset = frame.unwrap().pos.get(&call.args()[i]).unwrap();
                                s.push_str(&format!("  lw t0, {}(sp)\n", offset));
                                s.push_str(&format!("  sw t0, {}(sp)\n", pos));
                            }
                        }
                    }
                }

                let callee = call.callee();
                let name = &program.unwrap().func(callee).name()[1..];
                s.push_str(&format!("  call {}\n", name));
                match frame.unwrap().pos.get(self) {
                    Some(offset) => {
                        s.push_str(&format!("  sw a0, {}(sp)\n", offset));
                    }
                    None => (),
                }
                s
            }
            _ => unreachable!(),
        }
    }
}
