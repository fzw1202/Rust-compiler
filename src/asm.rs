use koopa::ir::ValueKind;

pub trait GenerateAsm {
    fn generate(&self, s: &mut String);
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, s: &mut String) {
        s.push_str("  .text\n");
        s.push_str("  .globl main\n");
        for &func in self.func_layout() {
            self.func(func).generate(s);
        }
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, s: &mut String) {
        s.push_str(&format!("{}:\n", &self.name()[1..]));
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                match value_data.kind() {
                    ValueKind::Integer(int) => {
                        s.push_str(&format!("{}\n", int.value()));
                    }
                    ValueKind::Return(ret) => {
                        let ret_value_data = self.dfg().value(ret.value().unwrap());
                        if let ValueKind::Integer(int) = ret_value_data.kind() {
                            s.push_str(&format!("  li a0, {}\n", int.value()));
                        }
                        s.push_str("  ret\n");
                    }
                    _ => unreachable!(),
                }
            }
        }
    }
}
