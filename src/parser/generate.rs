use super::ast::*;
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

pub fn generate(prog: Program) -> String {
    let mut asm = Assembly::new();
    match prog {
        Program { func, globals } => {
            for f in func {
                asm.add(gen_function(f));
            }
            asm.add(".data");
            for g in globals {
                asm.add(format!("_{0}: .word 0", g));
            }
        },
    };

    asm.build()
}

fn gen_function(fun: Function) -> Assembly {
    let mut asm = Assembly::new();
    match fun {
        Function { name, arguments, statements, variables } => {
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

            asm.add(format!(".global _{0}", name));
            asm.add(format!("_{0}:", name));
            asm.add("pushl %ebp");
            asm.add("movl %esp, %ebp");
            asm.add(format!("subl	${}, %esp", stack_size(&var_map)));

            for statement in statements {
                asm.add(gen_statement(statement, &var_map));
            }
        }
    }

    asm
}

fn gen_statement(stat: Statement, var_map: &HashMap<String, i32>) -> Assembly {
    let mut asm = Assembly::new();

    match stat {
        Statement::Return(exp) => {
            asm.add(gen_expression(exp, var_map));
            asm.add(format!("addl	${}, %esp", stack_size(var_map)));
            asm.add("popl %ebp");
            asm.add("ret\n");
        }
        Statement::Declare(name, exp) | Statement::Assign(name, exp) => {
            asm.add(gen_expression(exp, var_map));
            asm.add(format!("movl %eax, {}(%ebp)", var_map.get(&name).expect("Variable not found")));
        },
    }
    asm
}

fn gen_expression(exp: Expression, var_map: &HashMap<String, i32>) -> Assembly {
    let mut asm = Assembly::new();
    match exp {
        Expression::Int(val) => asm.add(format!("movl ${}, %eax", val)),
        Expression::UnOp(op, exp) => {
            asm.add(gen_expression(*exp, var_map));
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
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("addl %ecx, %eax");
                },
                BinOp::Subtraction => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("subl %ecx, %eax");
                },
                BinOp::Multiplication => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("imul %ecx, %eax");
                },
                BinOp::Division => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("xor %edx, %edx");
                    asm.add("idivl %ecx");
                }
                BinOp::Modulus => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("xor %edx, %edx");
                    asm.add("idivl %ecx");
                    asm.add("movl %edx, %eax");
                },
                BinOp::Equal => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("cmpl %ecx, %eax");
                    asm.add("sete %al");
                },
                BinOp::NotEqual => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("cmpl %ecx, %eax");
                    asm.add("setne %al");
                },
                BinOp::LessThan => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("cmpl %ecx, %eax");
                    asm.add("setl %al");
                },
                BinOp::LessThanOrEqual => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("cmpl %ecx, %eax");
                    asm.add("setle %al");
                },
                BinOp::GreaterThan => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("cmpl %ecx, %eax");
                    asm.add("setg %al");
                },
                BinOp::GreaterThanOrEqual => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("cmpl %ecx, %eax");
                    asm.add("setge %al");
                },
                BinOp::Or => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("orl %ecx, %eax");
                    asm.add("setne %al");
                },
                BinOp::And => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("cmpl $0, %ecx");
                    asm.add("setne %cl");
                    asm.add("cmpl $0, %eax");
                    asm.add("setne %al");
                    asm.add("andb %cl, %al");
                },
                BinOp::BitwiseLeft => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("shll %cl, %eax");
                },
                BinOp::BitwiseRight => {
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("pop %ecx");
                    asm.add("shrl %cl, %eax");
                },
                BinOp::BitwiseAnd => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("andl %ecx, %eax");
                },
                BinOp::BitwiseOr => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("orl %ecx, %eax");
                },
                BinOp::BitwiseXor => {
                    asm.add(gen_expression(*exp1, var_map));
                    asm.add("push %eax");
                    asm.add(gen_expression(*exp2, var_map));
                    asm.add("pop %ecx");
                    asm.add("xorl %ecx, %eax");
                },
            }
        },
        Expression::Variable(name) => {
            asm.add(format!("movl {}(%ebp), %eax", var_map.get(&name).expect("variable not found")))
        },
        Expression::FunctionCall(name, arguments) => {
            let mut i = -4;
            for exp in arguments {
                i += 4;
                asm.add(gen_expression(exp, var_map));
                asm.add(format!("movl %eax, {}(%esp)", i));
            }
            asm.add(format!("call _{}", name));
        }
    }
    asm
}

fn stack_size(var_map: &HashMap<String, i32>) -> i32 {
    16 + var_map.values().max().unwrap_or(&0)
}