use super::ast::*;

pub fn generate(prog: Program) -> String {
    match prog {
        Program { func } => gen_function(func),
    }
}

fn gen_function(fun: Function) -> String {
    match fun {
        Function { name, statement } => format!(".global _{0}\n_{0}:\n{1}", name, gen_statement(statement)),
    }
}

fn gen_statement(stat: Statement) -> String {
    match stat {
        Statement::Return(exp) => gen_expression(exp),
    }
}

fn gen_expression(exp: Expression) -> String {
    match exp {
        Expression::Int(val) => format!("movl ${}, %eax\nret", val),
    }
}