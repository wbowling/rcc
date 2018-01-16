use super::ops::*;
use std::collections::HashMap;

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
            Function { name, arguments, statements, variables } => {
                let stack_size = variables.len() * 4;
                let var_map = Generator::get_var_map(arguments, variables);

                asm.add(format!(".global _{0}", name));
                asm.add(format!("_{0}:", name));
                asm.add("pushl %ebp");
                asm.add("movl %esp, %ebp");
                asm.add(format!("subl ${}, %esp", stack_size));

                let mut has_return: bool = statements.iter().any(|s| if let Statement::Return(_) = *s { true } else { false });
                for statement in statements {
                    asm.add(self.gen_statement(statement, &var_map));
                }
                if !has_return {
                    asm.add("mov	%ebp, %esp");
                    asm.add("popl %ebp");
                    asm.add("ret\n");
                }
            }
        }

        asm
    }

    fn gen_statement(&mut self, stat: Statement, var_map: &HashMap<String, i32>) -> Assembly {
        let mut asm = Assembly::new();

        match stat {
            Statement::Return(exp) => {
                asm.add(self.gen_expression(exp, var_map));
                asm.add("mov	%ebp, %esp");
                asm.add("popl %ebp");
                asm.add("ret\n");
            }
            Statement::Declare(name, Some(exp)) => {
                asm.add(self.gen_expression(exp, var_map));
                asm.add(format!("movl %eax, {}(%ebp)", var_map.get(&name).expect("Variable not found")));
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
                asm.add("cmpb $0, %al");
                asm.add(format!("je .FALSE{}", num));
                asm.add(self.gen_statement(*true_body, var_map));
                asm.add(format!("jmp .END{}", num));
                asm.add(format!(".FALSE{}:", num));
                if let Some(false_body) = false_body_option {
                    asm.add(self.gen_statement(*false_body, var_map));
                }
                asm.add(format!(".END{}:", num));
            },
            Statement::Declare(_, None) => (),
        }
        asm
    }

    fn gen_expression(&mut self, exp: Expression, var_map: &HashMap<String, i32>) -> Assembly {
        let mut asm = Assembly::new();
        match exp {
            Expression::Int(val) => asm.add(format!("movl ${}, %eax", val)),
            Expression::UnOp(op, exp) => {
                asm.add(self.gen_expression(*exp, var_map));
                match op {
                    UnOp::Negation => asm.add("neg %eax"),
                    UnOp::BitComp => asm.add("not %eax"),
                    UnOp::LogicalNeg => asm.add_all(vec![
                        "cmpl $0, %eax",
                        "movl $0, %eax",
                        "sete %al"
                    ]),
                }
            },
            Expression::BinOp(op, exp1, exp2) => {
                match op {
                    BinOp::Addition => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("addl %ecx, %eax");
                    },
                    BinOp::Subtraction => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("subl %ecx, %eax");
                    },
                    BinOp::Multiplication => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("imul %ecx, %eax");
                    },
                    BinOp::Division => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("xor %edx, %edx");
                        asm.add("idivl %ecx");
                    }
                    BinOp::Modulus => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("xor %edx, %edx");
                        asm.add("idivl %ecx");
                        asm.add("movl %edx, %eax");
                    },
                    BinOp::Equal => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("cmpl %ecx, %eax");
                        asm.add("sete %al");
                    },
                    BinOp::NotEqual => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("cmpl %ecx, %eax");
                        asm.add("setne %al");
                    },
                    BinOp::LessThan => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("cmpl %ecx, %eax");
                        asm.add("setl %al");
                    },
                    BinOp::LessThanOrEqual => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("cmpl %ecx, %eax");
                        asm.add("setle %al");
                    },
                    BinOp::GreaterThan => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("cmpl %ecx, %eax");
                        asm.add("setg %al");
                    },
                    BinOp::GreaterThanOrEqual => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("cmpl %ecx, %eax");
                        asm.add("setge %al");
                    },
                    BinOp::Or => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("orl %ecx, %eax");
                        asm.add("setne %al");
                    },
                    BinOp::And => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("cmpl $0, %ecx");
                        asm.add("setne %cl");
                        asm.add("cmpl $0, %eax");
                        asm.add("setne %al");
                        asm.add("andb %cl, %al");
                    },
                    BinOp::BitwiseLeft => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("shll %cl, %eax");
                    },
                    BinOp::BitwiseRight => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("pop %ecx");
                        asm.add("shrl %cl, %eax");
                    },
                    BinOp::BitwiseAnd => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("andl %ecx, %eax");
                    },
                    BinOp::BitwiseOr => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("orl %ecx, %eax");
                    },
                    BinOp::BitwiseXor => {
                        asm.add(self.gen_expression(*exp1, var_map));
                        asm.add("push %eax");
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add("pop %ecx");
                        asm.add("xorl %ecx, %eax");
                    },
                    BinOp::Comma => {
                        asm.add(self.gen_expression(*exp2, var_map));
                        asm.add(self.gen_expression(*exp1, var_map));
                    },
                }
            },
            Expression::Variable(name) => {
                asm.add(format!("movl {}(%ebp), %eax", var_map.get(&name).expect("variable not found")))
            },
            Expression::VariableRef(name) => {
                asm.add(format!("leal {}(%ebp), %eax", var_map.get(&name).expect("variable not found")))
            },
            Expression::FunctionCall(name, arguments) => {
                let restore_size = 4 * arguments.len();
                for exp in arguments.into_iter().rev() {
                    asm.add(self.gen_expression(exp, var_map));
                    asm.add("push %eax");
                }
                asm.add(format!("call _{}", name));
                asm.add(format!("addl ${}, %esp", restore_size));
            },
            Expression::Assign(name, exp) => {
                asm.add(self.gen_expression(*exp, var_map));
                asm.add(format!("movl %eax, {}(%ebp)", var_map.get(&name).expect("Variable not found")));
            },
            Expression::AssignPostfix(name, exp) => {
                asm.add(format!("movl {}(%ebp), %eax", var_map.get(&name).expect("Variable not found")));
                asm.add("push %eax");
                asm.add(self.gen_expression(*exp, var_map));
                asm.add(format!("movl %eax, {}(%ebp)", var_map.get(&name).expect("Variable not found")));
                asm.add("pop %eax")
            },
            Expression::Ternary(condition, true_body, false_body) => {
                self.conditional_count += 1;
                let num = self.conditional_count;
                asm.add(self.gen_expression(*condition, var_map));
                asm.add("cmpb $0, %al");
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

    fn get_var_map(arguments: Vec<String>, variables: Vec<String>) -> HashMap<String, i32> {
        let mut var_map: HashMap<String, i32> = HashMap::new();

        let mut i = 8;

        for var in arguments {
            var_map.insert(var, i);
            i += 4;
        }

        i = -4;
        for var in variables {
            var_map.insert(var, i);
            i -= 4;
        }

        var_map
    }
}