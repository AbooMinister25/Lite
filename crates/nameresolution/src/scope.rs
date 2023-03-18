//! This module defines the Scope struct. The `Scope` struct
//! represents all the definitions in a scope.

use parser::ast::Statement;
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
    pub fn get(&self, key: &str) -> Option<&Definition> {
        self.definitions.get(&key.to_string())
    }
}
