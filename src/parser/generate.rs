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
                BinOp::Equal => vec![
                    gen_expression(*exp1),
                    s("push %eax"),
                    gen_expression(*exp2),
                    s("pop %ecx"),
                    s("cmpl %ecx, %eax"),
                    s("sete %al"),
                ].concat(),
                BinOp::NotEqual => vec![
                    gen_expression(*exp1),
                    s("push %eax"),
                    gen_expression(*exp2),
                    s("pop %ecx"),
                    s("cmpl %ecx, %eax"),
                    s("setne %al"),
                ].concat(),
                BinOp::LessThan => vec![
                    gen_expression(*exp2),
                    s("push %eax"),
                    gen_expression(*exp1),
                    s("pop %ecx"),
                    s("cmpl %ecx, %eax"),
                    s("setl %al"),
                ].concat(),
                BinOp::LessThanOrEqual => vec![
                    gen_expression(*exp2),
                    s("push %eax"),
                    gen_expression(*exp1),
                    s("pop %ecx"),
                    s("cmpl %ecx, %eax"),
                    s("setle %al"),
                ].concat(),
                BinOp::GreaterThan => vec![
                    gen_expression(*exp2),
                    s("push %eax"),
                    gen_expression(*exp1),
                    s("pop %ecx"),
                    s("cmpl %ecx, %eax"),
                    s("setg %al"),
                ].concat(),
                BinOp::GreaterThanOrEqual => vec![
                    gen_expression(*exp2),
                    s("push %eax"),
                    gen_expression(*exp1),
                    s("pop %ecx"),
                    s("cmpl %ecx, %eax"),
                    s("setge %al"),
                ].concat(),
                BinOp::Or => vec![
                    gen_expression(*exp1),
                    s("push %eax"),
                    gen_expression(*exp2),
                    s("pop %ecx"),
                    s("orl %ecx, %eax"),
                    s("setne %al"),
                ].concat(),
                BinOp::And => vec![
                    gen_expression(*exp1),
                    s("push %eax"),
                    gen_expression(*exp2),
                    s("pop %ecx"),
                    s("cmpl $0, %ecx"),
                    s("setne %cl"),
                    s("cmpl $0, %eax"),
                    s("setne %al"),
                    s("andb %cl, %al"),
                ].concat(),
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
