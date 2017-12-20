use super::ast::*;

pub fn generate(prog: Program) -> String {
    match prog {
        Program { func } => func.into_iter().map(|a| { gen_function(a) }).collect(),
    }
}

fn gen_function(fun: Function) -> String {
    match fun {
        Function { name, statement } => format!(".global _{0}\n_{0}:\n{1}", name, gen_statement(statement)),
    }
}

fn gen_statement(stat: Statement) -> String {
    match stat {
        Statement::Return(exp) => format!("{}ret\n", gen_expression(exp)),
    }
}

fn gen_expression(exp: Expression) -> String {
    match exp {
        Expression::Int(val) => format!("movl ${}, %eax\n", val),
        Expression::UnOp(op, exp) => {
            let asm = match op {
                UnOp::Negation => "neg %eax\n",
                UnOp::BitComp => "not %eax\n",
                UnOp::LogicalNeg => "cmpl $0, %eax\nmovl $0, %eax\nsete %al\n",
            };
            format!("{}{}", gen_expression(*exp), asm)
        },
        Expression::BinOp(op, exp1, exp2) => {
            match op {
                BinOp::Addition => format!("{}push %eax\n{}pop %ecx\n addl %ecx, %eax\n", gen_expression(*exp1), gen_expression(*exp2)),
                BinOp::Subtraction => format!("{}push %eax\n{}pop %ecx\n subl %ecx, %eax\n", gen_expression(*exp2), gen_expression(*exp1)),
                BinOp::Multiplication => format!("{}push %eax\n{}pop %ecx\n imul %ecx, %eax\n", gen_expression(*exp1), gen_expression(*exp2)),
                BinOp::Division => format!("{}push %eax\n{}pop %ecx\n xor %edx, %edx\nidivl %ecx\n", gen_expression(*exp2), gen_expression(*exp1)),
            }
        }
    }
}
