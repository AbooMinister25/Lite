//! This crate contains the logic for handling name resolution. This is the
//! process in which variables are resolved by finding what declaration it might
//! refer to in the context of the current scope.

#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::missing_const_for_fn)]
#![allow(clippy::return_self_not_must_use)]
#![allow(clippy::module_name_repetitions)]

pub mod scope;

use parser::ast::{Expr, Statement};
use span::Spanned;

use crate::scope::Scope;

#[derive(Default)]
pub struct NameResolver {
    scopes: Vec<Scope>,
}

impl NameResolver {
    pub fn resolve_names(&mut self, ast: Vec<Spanned<Statement>>) {
        for node in ast {
            self.resolve(node.0);
        }
    }

    fn resolve(&mut self, node: Statement) {
        match node {
            Statement::Expression(e) => self.resolve_expr(e.0),
            _ => (),
        }
    }

    fn resolve_expr(&mut self, expr: Expr) {
        match expr {
            Expr::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts {
                    self.resolve(stmt.0);
                }
                self.end_scope();
            }
            _ => (),
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
