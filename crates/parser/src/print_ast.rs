use crate::ast::{Expr, Statement};
use span::Spanned;

fn pad_string(string: &str, pad: usize) -> String {
    format!("{:>pad$}", string, pad = pad + string.len())
}

fn pretty_expr(expr: Expr, pad: usize) -> String {
    let ret = match expr {
        Expr::Literal(e) => e.to_string(),
        Expr::Ident(i) => format!("(Ident {})", i),
        Expr::Tuple(v) => format!(
            "(Tuple {})",
            v.iter()
                .map(|i| i.0.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        ),
        Expr::Array(v) => format!(
            "(Array {})",
            v.iter()
                .map(|i| i.0.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        ),
        Expr::Unary { op, rhs } => format!("(Unary ({}{}))", op, rhs.0),
        Expr::Binary { op, lhs, rhs } => format!("(Binary ({} {} {}))", lhs.0, op, rhs.0),
        Expr::Call { callee, args } => format!(
            "(Call {}({}) )",
            callee.0,
            args.iter()
                .map(|i| i.0.to_string())
                .collect::<Vec<String>>()
                .join(", ")
        ),
        Expr::Assignment { name, op, value } => {
            format!("(Assignment {} {} {})", name.0, op, value.0)
        }
        Expr::Block(c) => {
            let expr_reprs = c
                .iter()
                .map(|e| pretty_statement(e.0.clone(), pad + 2))
                .collect::<Vec<String>>();

            format!("(Block \n{})", expr_reprs.join("\n"))
        }
        Expr::If {
            condition,
            body,
            else_,
        } if else_.is_none() => {
            let mut cond_repr = pad_string("condition ", pad + 2);
            cond_repr.push_str(&pretty_expr(condition.0, 0));

            let body_repr = format!(
                "{} (\n{})",
                pad_string("body", pad + 2),
                pretty_expr(body.0, pad + 4),
            );

            format!("(If\n{}\n{})", cond_repr, body_repr)
        }
        Expr::If {
            condition,
            body,
            else_,
        } => {
            let mut cond_repr = pad_string("condition ", pad + 2);
            cond_repr.push_str(&pretty_expr(condition.0, 0));

            let body_repr = format!(
                "{} (\n{})",
                pad_string("body", pad + 2),
                pretty_expr(body.0, pad + 4)
            );

            let else_repr = format!(
                "{} (\n{})",
                pad_string("else", pad + 2),
                pretty_expr(else_.unwrap().0, pad + 4)
            );

            format!("(If\n{}\n{}\n{})", cond_repr, body_repr, else_repr)
        }
        Expr::For { var, iter, body } => {
            let mut var_repr = pad_string("var ", pad + 2);
            var_repr.push_str(&pretty_expr(var.0, 0));
            let mut iter_repr = pad_string("iter ", pad + 2);
            iter_repr.push_str(&pretty_expr(iter.0, 0));

            let body_repr = format!(
                "{} (\n{})",
                pad_string("body", pad + 2),
                pretty_expr(body.0, pad + 4)
            );

            format!("(For\n{}\n{}\n{})", var_repr, iter_repr, body_repr)
        }
        _ => todo!(),
    };

    pad_string(&ret, pad)
}

fn pretty_statement(stmt: Statement, pad: usize) -> String {
    let ret = match stmt {
        Statement::Expression(e) => {
            let expr = pretty_expr(e.0, pad + 2);
            format!("(Expression\n{})", expr)
        }
        Statement::Return(e) => format!("(Return {})", e.0),
        Statement::Import(e) => format!("(Import {})", e.0),
        Statement::Let {
            name,
            value,
            mutable,
        } if mutable => {
            let name = pretty_expr(name.0, 0);
            let value = pretty_expr(value.0, 0);
            let mutable = pad_string(&mutable.to_string(), 0);

            format!(
                "Let(\n{} {}\n{} {}\n{} {})",
                pad_string("name", pad + 2),
                name,
                pad_string("mutable", pad + 2),
                mutable,
                pad_string("value", pad + 2),
                value
            )
        }
        Statement::Function {
            name,
            public,
            params,
            annotations,
            return_annotation,
            body,
        } => {
            let mut name_repr = pad_string("name ", pad + 2);
            name_repr.push_str(&pretty_expr(name.0, 0));
            let public_repr = pad_string(&format!("public {public}"), pad + 2);
            let params_repr = pad_string(&format!("params ({})", params.join(", ")), pad + 2);

            let pretty_annotations = annotations
                .iter()
                .map(|i| i.0.to_string())
                .collect::<Vec<String>>()
                .join(", ");
            let annotations_repr =
                pad_string(&format!("annotations ({pretty_annotations})"), pad + 2);

            let pretty_return = if let Some((annotation, _)) = return_annotation {
                annotation.to_string()
            } else {
                "()".to_string()
            };
            let return_repr = pad_string(&format!("return_annotation {pretty_return}"), pad + 2);

            let body_repr = format!(
                "{} (\n{})",
                pad_string("body", pad + 2),
                pretty_expr(body.0, pad + 4),
            );

            format!(
                "(Func\n{}\n{}\n{}\n{}\n{}\n{})",
                name_repr, public_repr, params_repr, annotations_repr, return_repr, body_repr
            )
        }
        _ => todo!()
    };

    ret
}

pub fn print_ast(nodes: Vec<Spanned<Statement>>) {
    let pad = 2;

    println!("(");
    for node in nodes {
        let ret = pretty_statement(node.0, pad);
        println!("{})", pad_string(&ret, pad));
    }
}
