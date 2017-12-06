use super::ast::*;
use super::token::Op;

pub fn generate(prog: Program) -> String {
    match prog {
        Program { func } => func.into_iter().map(|a| { gen_function(a) }).collect(),
    }
}

fn gen_function(fun: Function) -> String {
    match fun {
        Function { name, statement } => format!(".global _{0}\n_{0}:\n{1}\n", name, gen_statement(statement)),
    }
}

fn gen_statement(stat: Statement) -> String {
    match stat {
        Statement::Return(exp) => format!("xor %eax, %eax\n{}\nret\n", gen_expression(exp, None))
    }
}

fn gen_expression(exp: Expression, op: Option<Op>) -> String {
    match exp {
        Expression::Int(val) => {
            match op {
                None | Some(Op::Add) => format!("add ${}, %eax", val),
                Some(Op::Sub) => format!("sub ${}, %eax", val),
            }
        },
        Expression::Expr(x, next_op, y) => format!("{}\n{}", gen_expression(*x, op), gen_expression(*y, Some(next_op))),
    }
}