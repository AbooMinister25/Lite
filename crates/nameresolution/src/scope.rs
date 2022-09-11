//! This module defines the Scope struct. The `Scope` struct
//! represents all the definitions in a scope.

use parser::ast::{Expr, Statement};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
pub struct Definition {
    pub node: Statement,
    pub initialized: bool,
}

/// Represents all definitions in a scope.
#[derive(Debug, PartialEq, Default)]
pub struct Scope {
    pub definitions: HashMap<String, Definition>,
}

impl Scope {
    //TODO: Can't create a `Default` implementation for the struct
    pub fn new() -> Self {
        Self {
            definitions: HashMap::new(),
        }
    }
}
