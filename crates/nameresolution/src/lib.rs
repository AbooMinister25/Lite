//! This crate contains the logic for handling name resolution. This is the
//! process in which variables are resolved by finding what declaration it might
//! refer to in the context of the current scope.

#![warn(clippy::pedantic, clippy::nursery)]
#![allow(clippy::must_use_candidate)]
#![allow(clippy::unnecessary_wraps)]
#![allow(clippy::missing_const_for_fn)]
#![allow(clippy::return_self_not_must_use)]
#![allow(clippy::module_name_repetitions)]

pub mod errors;
pub mod scope;

use parser::ast::{Expr, Statement};
use span::Spanned;

use crate::errors::{ErrorKind, ResolutionError};
use crate::scope::Scope;

/// Resolves names in a program from its AST.
#[derive(Default)]
pub struct NameResolver {
    scopes: Vec<Scope>,
}

impl NameResolver {
    /// Resolve the names in a given AST.
    ///
    /// # Errors
    /// Returns an Err(ResolutionError) in the case that an error occurs
    /// during name resolution
    pub fn resolve_names(&mut self, ast: Vec<Spanned<Statement>>) -> Result<(), ResolutionError> {
        for node in ast {
            self.resolve(node.0)?;
        }

        Ok(())
    }

    fn resolve(&mut self, node: Statement) -> Result<(), ResolutionError> {
        match node {
            Statement::Expression(e) => self.resolve_expr(e)?,
            _ => (),
        }

        Ok(())
    }

    fn resolve_expr(&mut self, expr: Spanned<Expr>) -> Result<(), ResolutionError> {
        match expr.0 {
            Expr::Ident(s) => {
                if !self.scopes.is_empty() {
                    // Safe to unwrap since scopes is confirmed to not be empty
                    let current = self.scopes.last().unwrap();
                    let symbol = current.get(&s);

                    // If variable being accessed inside its own initializer,
                    // return an error.
                    if let Some(d) = symbol {
                        if !d.initialized {
                            return Err(ResolutionError::new(
                                ErrorKind::ReadLocalInInit(s, expr.1),
                                "Can't read local variable in its own initializer".to_string(),
                                None,
                            ));
                        }
                    }

                    
                }
            }
            Expr::Block(stmts) => {
                self.begin_scope();
                for stmt in stmts {
                    self.resolve(stmt.0)?;
                }
                self.end_scope();
            }
            _ => (),
        }

        Ok(())
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }
}
