use super::ops::*;
use std::collections::HashMap;

enum ProgVariable {
    StackVariable(Size, i32),
}

struct Assembly {
    asm: Vec<String>,
}

impl Into<String> for Assembly {
    fn into(self) -> String {
        self.build()
    }
}

impl Assembly {
    fn new() -> Assembly {
        Assembly { asm: vec![] }
    }

    fn add<S: Into<String>>(&mut self, string: S) {
        self.asm.push(string.into())
    }

    fn add_all<S: Into<String>>(&mut self, strings: Vec<S>) {
        for string in strings {
            self.asm.push(string.into())
        }
    }

    fn build(&self) -> String {
        self.asm.join("\n")
    }
}

pub struct Generator {
    conditional_count: i32
}

impl Generator {
    pub fn new() -> Generator {
        Generator { conditional_count: 0 }
    }

    pub fn generate(&mut self, prog: Program) -> String {
        self.conditional_count = 0;
        let mut asm = Assembly::new();
        match prog {
            Program { func, globals } => {
                asm.add(".intel_syntax noprefix");
                asm.add(".text");

                for f in func {
                    asm.add(self.gen_function(f));
                }
                asm.add(".data");
                for g in globals {
                    asm.add(format!("_{0}: .word 0", g));
                }
            },
        };

        asm.build()
    }

    fn gen_function(&mut self, fun: Function) -> Assembly {
        let mut asm = Assembly::new();
        match fun {
            Function { name, arguments, statements } => {

                let (var_map, stack_size) = Generator::get_var_sizes(&statements, &arguments);

                asm.add(format!(".globl _{}", name));
                asm.add(format!("_{}:", name));
                asm.add("push rbp");
                asm.add("mov rbp, rsp");
                asm.add(format!("sub rsp, {}", stack_size));
                asm.add(self.gen_func_args(&arguments));

                let mut has_return: bool = statements.iter().any(|s| if let Statement::Return(_) = *s { true } else { false });
                for statement in statements {
                    asm.add(self.gen_statement(statement, &var_map));
                }
                if !has_return {
                    asm.add("mov	rsp, rbp");
                    asm.add("pop rbp");
                    asm.add("ret\n");
                }
            }
        }

        asm
    }

    fn gen_func_args(&mut self, arguments: &Vec<Variable>) -> Assembly {
        let mut asm = Assembly::new();
        let regs = vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"];
        let mut i: usize = 0;
        for arg in arguments {
            if i < 6 {
                match arg {
                    &Variable { ref size, .. } => {
                        match size {
                            &Size::Int => asm.add(format!("mov DWORD PTR[rbp + {}], {}", (i as i32 + 1) * -8, Generator::reg_dword(regs[i]))),
                            &Size::Byte => asm.add(format!("mov BYTE PTR[rbp + {}], {}", (i as i32 + 1) * -8, Generator::reg_byte(regs[i]))),
                        }
                        i += 1;
                    }
                }
            }
        }
        asm
    }

    fn gen_statement(&mut self, stat: Statement, var_map: &HashMap<String, ProgVariable>) -> Assembly {
        let mut asm = Assembly::new();

        match stat {
            Statement::Return(exp) => {
                asm.add(self.gen_expression(exp, var_map));
                asm.add("mov	rsp, rbp");
                asm.add("pop rbp");
                asm.add("ret\n");
            }
            Statement::Declare(Variable { name, .. }, Some(exp)) => {
                asm.add(self.gen_expression(exp, var_map));
                asm.add(self.gen_set_variable(&name, var_map));
            },
            Statement::Exp(exp) => asm.add(self.gen_expression(exp, var_map)),
            Statement::Compound(statements) => {
                for statement in statements {
                    asm.add(self.gen_statement(statement, var_map))
                }
            },
            Statement::If(condition, true_body, false_body_option) => {
                self.conditional_count += 1;
                let num = self.conditional_count;
                asm.add(self.gen_expression(condition, var_map));
                asm.add("cmp al, 0");
                asm.add(format!("je .FALSE{}", num));
                asm.add(self.gen_statement(*true_body, var_map));
                asm.add(format!("jmp .END{}", num));
                asm.add(format!(".FALSE{}:", num));
                if let Some(false_body) = false_body_option {
                    asm.add(self.gen_statement(*false_body, var_map));
                }
                asm.add(format!(".END{}:", num));
            },
            Statement::While(condition, while_body) => {
                self.conditional_count += 1;
                let num = self.conditional_count;
                asm.add(format!(".WHILE_START{}:", num));
                asm.add(self.gen_expression(condition, var_map));
                asm.add("cmp al, 0");
                asm.add(format!("je .WHILE_END{}", num));
                asm.add(self.gen_statement(*while_body, var_map));
                asm.add(format!("jmp .WHILE_START{}", num));
                asm.add(format!(".WHILE_END{}:", num));
            }
            Statement::Declare(_, None) => (),
        }
        asm
    }

    fn gen_expression(&mut self, exp: Expression, var_map: &HashMap<String, ProgVariable>) -> Assembly {
        let mut asm = Assembly::new();
        match exp {
            Expression::Int(val) => asm.add(format!("mov eax, {}", val)),
            Expression::Char(val) => asm.add(format!("mov al, {}", val)),
            Expression::UnOp(op, exp) => {
                asm.add(self.gen_expression(*exp, var_map));
                match op {
                    UnOp::Negation => asm.add("neg rax"),
                    UnOp::BitComp => asm.add("not rax"),
                    UnOp::LogicalNeg => asm.add_all(vec![
                        "cmp rax, 0",
                        "mov rax, 0",
                        "sete al"
                    ]),
                }
            },
            Expression::BinOp(op, exp1, exp2) => {
                match op {
                    BinOp::Addition => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("add rax, rcx");
                    },
                    BinOp::Subtraction => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("sub rax, rcx");
                    },
                    BinOp::Multiplication => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("imul rax, rcx");
                    },
                    BinOp::Division => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("xor rdx, rdx");
                        asm.add("idiv rcx");
                    }
                    BinOp::Modulus => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("xor rdx, rdx");
                        asm.add("idiv rcx");
                        asm.add("mov rax, rdx");
                    },
                    BinOp::Equal => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("cmp rax, rcx");
                        asm.add("sete al");
                    },
                    BinOp::NotEqual => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("cmp rax, rcx");
                        asm.add("setne al");
                    },
                    BinOp::LessThan => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("cmp rax, rcx");
                        asm.add("setl al");
                    },
                    BinOp::LessThanOrEqual => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("cmp rax, rcx");
                        asm.add("setle al");
                    },
                    BinOp::GreaterThan => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("cmp rax, rcx");
                        asm.add("setg al");
                    },
                    BinOp::GreaterThanOrEqual => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("cmp rax, rcx");
                        asm.add("setge al");
                    },
                    BinOp::Or => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("or rax, rcx");
                        asm.add("setne al");
                    },
                    BinOp::And => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("cmp rcx, 0");
                        asm.add("setne cl");
                        asm.add("cmp rax, 0");
                        asm.add("setne al");
                        asm.add("and al, cl");
                    },
                    BinOp::BitwiseLeft => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("shl rax, cl");
                    },
                    BinOp::BitwiseRight => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop rcx");
                        asm.add("shr rax, cl");
                    },
                    BinOp::BitwiseAnd => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("and rax, rcx");
                    },
                    BinOp::BitwiseOr => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("or rax, rcx");
                    },
                    BinOp::BitwiseXor => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push rax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop rcx");
                        asm.add("xor rax, rcx");
                    },
                    BinOp::Comma => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add(self.gen_expression(*exp1, var_map));
                    },
                }
            },
            Expression::Variable(name) => {
                asm.add(self.gen_get_variable(&name, var_map))
            },
            Expression::VariableRef(name) => {
                match var_map.get(&name) {
                    Some(&ProgVariable::StackVariable(_, ref index)) => {
                        asm.add(format!("lea rax, QWORD PTR[rbp + {}]", index));
                    },
                    None => panic!("variable not found")
                }
            },
            Expression::FunctionCall(name, arguments) => {
                let extra = arguments.len() as i32 - 6;
                let regs = vec!["rdi", "rsi", "rdx", "rcx", "r8", "r9"];

                if extra > 0 {
                    let restore_size = 8 * ((extra + 1) & !0x01);
                    asm.add(format!("sub rsp, {}", restore_size));
                }

                let mut i = 0;
                for exp in arguments.into_iter() {
                    asm.add(self.gen_expression(exp, var_map));
                    if i > 5 {
                        asm.add(format!("mov QWORD PTR[rsp + {}], rax", (i - 6)*8));
                    } else {
                        asm.add(format!("mov {}, rax", regs[i]));
                    }
                    i += 1;
                }

                asm.add(format!("call _{}", name));

                if extra > 0 {
                    let restore_size = 8 * ((extra + 1) & !0x01);
                    asm.add(format!("add rsp, {}", restore_size));
                }
            },
            Expression::Assign(name, exp) => {
                asm.add(self.gen_expression(*exp, var_map));
                asm.add(self.gen_set_variable(&name, var_map));
            },
            Expression::AssignPostfix(name, exp) => {
                asm.add(self.gen_get_variable(&name, var_map));
                asm.add("push rax");
                asm.add(self.gen_expression(*exp, var_map));
                asm.add(self.gen_set_variable(&name, var_map));
                asm.add("pop rax")
            },
            Expression::Ternary(condition, true_body, false_body) => {
                self.conditional_count += 1;
                let num = self.conditional_count;
                asm.add(self.gen_expression(*condition, var_map));
                asm.add("cmp al, 0");
                asm.add(format!("je .FALSE{}", num));
                asm.add(self.gen_expression(*true_body, var_map));
                asm.add(format!("jmp .END{}", num));
                asm.add(format!(".FALSE{}:", num));
                asm.add(self.gen_expression(*false_body, var_map));
                asm.add(format!(".END{}:", num));
            },
        }
        asm
    }

    fn gen_set_variable(&self, name: &String, var_map: &HashMap<String, ProgVariable>) -> Assembly {
        let mut asm = Assembly::new();
        match var_map.get(name) {
            Some(&ProgVariable::StackVariable(ref size, ref index)) => match size {
                &Size::Int => asm.add(format!("mov DWORD PTR[rbp + {}], eax", index)),
                &Size::Byte => asm.add(format!("mov BYTE PTR[rbp + {}], al", index)),
            }
            None => panic!("Variable {} not found", name)
        }
        asm
    }

    fn gen_get_variable(&self, name: &String, var_map: &HashMap<String, ProgVariable>) -> Assembly {
        let mut asm = Assembly::new();
        match var_map.get(name) {
            Some(&ProgVariable::StackVariable(ref size, ref index)) => match size {
                &Size::Int => asm.add(format!("mov eax, DWORD PTR[rbp + {}]", index)),
                &Size::Byte => asm.add(format!("mov al, BYTE PTR[rbp + {}]", index)),
            },
            None => panic!("Variable {} not found", name)
        }
        asm
    }


    fn get_var_sizes(statements: &Vec<Statement>, arguments: &Vec<Variable>) -> (HashMap<String, ProgVariable>, u32) {
        let mut var_map: HashMap<String, ProgVariable> = HashMap::new();

        let mut r = 0;
        let mut stack_size = 0;
        let mut i = -8;

        for arg in arguments {
            match arg {
                &Variable { ref name, ref size } => {
                    Generator::check_var(&var_map, &name);
                    let s = match size {
                        &Size::Int => Size::Int,
                        &Size::Byte => Size::Byte
                    };
                    if r > 5 {
                        var_map.insert(name.clone(), ProgVariable::StackVariable(s, (r-4)*8));
                    } else {
                        var_map.insert(name.clone(), ProgVariable::StackVariable(s, i));
                        i -= 8;
                        stack_size += 1;
                    }
                    r += 1;
                }
            }
        }

        for statement in statements {
            match statement {
                &Statement::Exp(Expression::Assign(ref name, _)) |
                &Statement::Exp(Expression::AssignPostfix(ref name, _)) |
                &Statement::Exp(Expression::Variable(ref name)) |
                &Statement::Exp(Expression::VariableRef(ref name)) => {
                    Generator::check_no_var(&var_map, name)
                },
                &Statement::Declare(Variable { ref name, ref size }, _) => {
                    Generator::check_var(&var_map, name);
                    match size {
                        &Size::Int => var_map.insert(name.clone(), ProgVariable::StackVariable(Size::Int,i)),
                        &Size::Byte => var_map.insert(name.clone(), ProgVariable::StackVariable(Size::Byte,i)),
                    };
                    i -= 8;
                    stack_size += 1;
                },
                _ => (),
            };
        }


        stack_size = 8 * ((stack_size + 1) & !0x01);
        (var_map, stack_size)
    }

    fn check_var(var_map: &HashMap<String, ProgVariable>, name: &String) {
        if var_map.get(name).is_some() {
            panic!("Variable {} already defined", name);
        }
    }

    fn check_no_var(var_map: &HashMap<String, ProgVariable>, name: &String) {
        if var_map.get(name).is_none() {
            panic!("Variable {} not defined", name);
        }
    }

    fn reg_dword(reg: &str) -> &str {
        match reg {
            "rdi" => "edi",
            "rsi" => "esi",
            "rdx" => "edx",
            "rcx" => "ecx",
            "r8" => "r8d",
            "r9" => "r9d",
            other => panic!("err convertion reg {}", other),
        }
    }

    fn reg_byte(reg: &str) -> &str {
        match reg {
            "rdi" => "dil",
            "rsi" => "sil",
            "rdx" => "dl",
            "rcx" => "cl",
            "r8" => "r8b",
            "r9" => "r9b",
            other => panic!("err convertion reg {}", other),
        }
    }
}
