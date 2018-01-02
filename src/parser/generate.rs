use super::ast::*;

pub fn generate(prog: Program) -> String {
    let asm: Vec<String> = match prog {
        Program { func } => func.into_iter().map(|a| { gen_function(a).join("\n") }).collect(),
    };

    asm.join("\n")
}

fn gen_function(fun: Function) -> Vec<String> {
    match fun {
        Function { name, statement } => {
            vec![
                vec![
                    format!(".global _{0}", name),
                    format!("_{0}:", name)
                ],
                gen_statement(statement)
            ].concat()
        }
    }
}

fn gen_statement(stat: Statement) -> Vec<String> {
    match stat {
        Statement::Return(exp) => vec![
            gen_expression(exp),
            s("ret")
        ].concat(),
    }
}

fn gen_expression(exp: Expression) -> Vec<String> {
    match exp {
        Expression::Int(val) => vec![format!("movl ${}, %eax", val)],
        Expression::UnOp(op, exp) => {
            let asm = match op {
                UnOp::Negation => s("neg %eax"),
                UnOp::BitComp => s("not %eax"),
                UnOp::LogicalNeg => sa(&[
                    "cmpl $0, %eax",
                    "movl $0, %eax",
                    "sete %al"
                ]),
            };
            vec![gen_expression(*exp), asm].concat()
        },
        Expression::BinOp(op, exp1, exp2) => {
            match op {
                BinOp::Addition => vec![
                    gen_expression(*exp1),
                    s("push %eax"),
                    gen_expression(*exp2),
                    s("pop %ecx"),
                    s("addl %ecx, %eax"),
                ].concat(),
                BinOp::Subtraction => vec![
                    gen_expression(*exp2),
                    s("push %eax"),
                    gen_expression(*exp1),
                    s("pop %ecx"),
                    s("subl %ecx, %eax"),
                ].concat(),
                BinOp::Multiplication => vec![
                    gen_expression(*exp1),
                    s("push %eax"),
                    gen_expression(*exp2),
                    s("pop %ecx"),
                    s("imul %ecx, %eax"),
                ].concat(),
                BinOp::Division => vec![
                    gen_expression(*exp2),
                    s("push %eax"),
                    gen_expression(*exp1),
                    s("pop %ecx"),
                    s("xor %edx, %edx"),
                    s("idivl %ecx"),
                ].concat(),
                _ => unimplemented!()
            }
        }
    }
}

fn s(string: &str) -> Vec<String> {
    vec![string.to_string()]
}

fn sa(string: &[&str]) -> Vec<String> {
    string.iter().map({|s| s.to_string() }).collect()
}
