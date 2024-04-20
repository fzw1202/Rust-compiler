use koopa::ir::ValueKind;
use koopa::ir::FunctionData;

pub trait GenerateAsm {
    fn generate(&self, s: &mut String, _func_data: Option<&FunctionData>);
}

impl GenerateAsm for koopa::ir::Program {
    fn generate(&self, s: &mut String, _func_data: Option<&FunctionData>) {
        s.push_str("  .text\n");
        s.push_str("  .globl main\n");
        for &func in self.func_layout() {
            self.func(func).generate(s, None);
        }
    }
}

impl GenerateAsm for koopa::ir::FunctionData {
    fn generate(&self, s: &mut String, _func_data: Option<&FunctionData>) {
        s.push_str(&format!("{}:\n", &self.name()[1..]));
        for (&_bb, node) in self.layout().bbs() {
            for &inst in node.insts().keys() {
                let value_data = self.dfg().value(inst);
                value_data.generate(s, Some(self));
            }
        }
    }
}


impl GenerateAsm for koopa::ir::entities::ValueData {
    fn generate(&self, s: &mut String, func_data: Option<&FunctionData>) {
        match self.kind() {
            ValueKind::Integer(int) => {
                s.push_str(&format!("{}", int.value()));
            }
            ValueKind::Return(ret) => {
                let ret_value_data = func_data.unwrap().dfg().value(ret.value().unwrap());
                s.push_str("  li a0, ");
                ret_value_data.generate(s, func_data);
                s.push_str("\n  ret\n");
            }
            _ => unreachable!(),
        }
    }
}