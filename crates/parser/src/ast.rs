//! The `AST` (Abstract Syntax Tree) makes up a tree like
//! representation of Lite's syntax. This file contains structures
//! to represent different parts of the AST. The most notable are the
//! `Expr` and `Statement` enums.

use lexer::Position;
use std::fmt;

/// Kinds of Literals
///
/// integers, floats, booleans, strings, identifiers, and chars.
#[derive(Debug, Clone, PartialEq)]
pub enum LiteralKind {
    /// Integer literal (`10`)
    Int(i64),
    /// Float literal (`3.5`)
    Float(f64),
    /// Boolean literal (`true`, `false`)
    Bool(bool),
    /// String literal (`"foo"`)
    String(String),
    /// Identifier literal (`bar`)
    Ident(String),
    /// Character literal (`'c'`)
    Char(char),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LiteralKind::Int(i) => i.to_string(),
                LiteralKind::Float(f) => f.to_string(),
                LiteralKind::Bool(b) => b.to_string(),
                LiteralKind::String(s) => s.to_string(),
                LiteralKind::Ident(i) => i.to_string(),
                LiteralKind::Char(c) => c.to_string(),
            }
        )
    }
}

/// Binary operators
#[derive(Debug, Clone, PartialEq)]
pub enum BinOpKind {
    /// `+` operator
    Add,
    /// `-` operator
    Sub,
    /// `*` operator
    Mul,
    /// `/` operator
    Div,
    /// `and` operator
    And,
    /// `or` operator
    Or,
    /// `==` operator
    Eq,
    /// `!=` operator
    Ne,
    /// `>` operator
    Gt,
    /// `>=` operator
    Ge,
    /// `<` operator
    Lt,
    /// `<=` operator
    Le,
}

impl fmt::Display for BinOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                BinOpKind::Add => "+",
                BinOpKind::Sub => "-",
                BinOpKind::Mul => "*",
                BinOpKind::Div => "/",
                BinOpKind::And => "and",
                BinOpKind::Or => "or",
                BinOpKind::Eq => "==",
                BinOpKind::Ne => "!=",
                BinOpKind::Gt => ">",
                BinOpKind::Ge => ">=",
                BinOpKind::Lt => "<",
                BinOpKind::Le => "<=",
            }
        )
    }
}

/// Unary operators
#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOpKind {
    /// `!` operator
    Not,
    /// `-` operator
    Neg,
}

impl fmt::Display for UnaryOpKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                UnaryOpKind::Not => "!",
                UnaryOpKind::Neg => "-",
            }
        )
    }
}

/// A range
///
/// `1..10`, `3..=9`
#[derive(Debug, Clone, PartialEq)]
pub struct Range {
    pub start: Expr,
    pub end: Expr,
    pub inclusive: bool,
}

impl fmt::Display for Range {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// The different patterns that Lite's pattern matching allows
#[derive(Debug, Clone, PartialEq)]
pub enum PatKind {
    /// Wildcard (`_`)
    Wild,
    /// Identifier (`foo`)
    Ident(String),
    /// Literal (`10`, `"Hello"`)
    Literal(LiteralKind),
    /// Or pattern (`A | B | C`)
    Or(Vec<PatKind>),
    /// Range pattern (`1..10`, `3..=9`)
    Range(Range),
    /// Tuple pattern (`(a, b, c)`)
    Tuple(Vec<PatKind>),
    /// Rest pattern (`(a, b, ..)`)
    Rest,
}

impl fmt::Display for PatKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                PatKind::Wild => "_".to_string(),
                PatKind::Ident(i) => i.to_string(),
                PatKind::Literal(l) => l.to_string(),
                PatKind::Or(o) => format!("{:?}", o),
                PatKind::Range(r) => r.to_string(),
                PatKind::Tuple(t) => format!("{:?}", t),
                PatKind::Rest => "..".to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct MatchArm {
    pub pattern: PatKind,
    pub guard: Option<Expr>,
    pub body: Expr,
}

impl fmt::Display for MatchArm {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "Pat[pattern: {}, guard: {:?}, body: {}]",
            self.pattern, self.guard, self.body
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum AnnotationKind {
    Single(String),
    Tuple(Vec<AnnotationKind>),
    Array(Vec<AnnotationKind>),
}

impl fmt::Display for AnnotationKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                AnnotationKind::Single(s) => s.to_string(),
                AnnotationKind::Tuple(t) => format!("{:?}", t),
                AnnotationKind::Array(a) => format!("{:?}", a),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Annotation {
    kind: AnnotationKind,
    name: String,
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}: {}", self.name, self.kind)
    }
}

/// A statement is some standalone unit which does something comprised of
/// one or more statements.
#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    /// An expression statement
    Expression { expr: Expr, position: Position },
    /// A return statement
    ///
    /// `return <expr>`
    Return { expr: Expr, position: Position },
    /// An import statement
    ///
    /// `import package`
    Import { name: String, position: Position },
    /// A `let` variable declaration
    ///
    /// `let [mut] <name> = <expr>`
    Let {
        name: String,
        value: Expr,
        mutable: bool,
        position: Position,
    },
    /// A function declaration
    ///
    /// `func <name>(<args>) do <expr> end`
    Function {
        name: String,
        params: Vec<String>,
        annotations: Vec<Annotation>,
        body: Expr,
        position: Position,
    },
    /// A class declaration
    ///
    /// `class <name> [is] [traits] [of] [superclass] do [fields] [methods] end`
    Class {
        name: String,
        superclass: Option<Expr>,
        traits: Vec<Expr>,
        fields: Vec<Expr>,
        instance_methods: Vec<Statement>,
        class_methods: Vec<Statement>,
        position: Position,
    },
    /// A trait declaration
    ///
    /// `trait <name> do [fields] [methods] end`
    Trait {
        name: String,
        attributes: Vec<Expr>,
        methods: Vec<Statement>,
        position: Position,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Statement::Expression { expr, .. } => expr.to_string(),
                Statement::Return { expr, .. } => format!("return {expr}"),
                Statement::Import { name, .. } => format!("return {name}"),
                Statement::Let {
                    name,
                    value,
                    mutable,
                    ..
                } if *mutable => format!("let mut {name} = {value}"),
                Statement::Let { name, value, .. } => format!("let {name} = {value}"),
                Statement::Function {
                    name,
                    params,
                    annotations: _,
                    body,
                    ..
                } => format!("func {name}({}) do {body}", params.join(",")),
                Statement::Class {
                    name,
                    superclass,
                    traits,
                    fields,
                    instance_methods,
                    class_methods,
                    ..
                } => format!(
                    "Class[name: {name}, traits: {:?}, superclass: {:?}, fields: {:?}, instance_methods: {:?}, class_methods: {:?}",
                    traits,
                    superclass,
                    fields,
                    instance_methods,
                    class_methods
                ),
                Statement::Trait {
                    name,
                    attributes,
                    methods,
                    ..
                } => format!("Trait[name: {name}, attributes: {:?}, methods: {:?}", attributes, methods),
            }
        )
    }
}

/// An expression is some item that evaluates to some value
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    /// Literals (`10`, `"Hi"`)
    Literal {
        value: LiteralKind,
        position: Position,
    },
    /// Tuples (`(1, 2, 3)`)
    Tuple {
        values: Vec<Expr>,
        position: Position,
    },
    /// Arrays (`[1, 2, 3]`)
    Array {
        values: Vec<Expr>,
        position: Position,
    },
    /// A grouping (`(5 + 5)`)
    Grouping { expr: Box<Expr>, position: Position },
    /// An unary operation (`!foo`, `-bar`)
    Unary {
        op: String,
        rhs: Box<Expr>,
        position: Position,
    },
    /// A binary operation (`5 + 5`)
    Binary {
        op: String,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        position: Position,
    },
    /// A function call (`foo()`)
    Call {
        name: String,
        args: Vec<Expr>,
        position: Position,
    },
    /// A variable assignment (`foo = 10`)
    Assignment {
        name: String,
        value: Box<Expr>,
        position: Position,
    },
    /// A block
    ///
    /// `do <code> end`
    Block {
        code: Vec<Statement>,
        position: Position,
    },
    /// An `if` expression
    ///
    /// `if <expr> do <code> else <code> end`
    If {
        condition: Box<Expr>,
        body: Vec<Expr>,
        _else: Option<Vec<Expr>>,
        position: Position,
    },
    /// A for loop
    ///
    /// `for i in it do <code> end`
    For {
        var: Box<Expr>,
        iter: Box<Expr>,
        body: Vec<Expr>,
        position: Position,
    },
    /// A while loop
    ///
    /// `while <expr> do <code> end`
    While {
        expr: Box<Expr>,
        body: Vec<Expr>,
        position: Position,
    },
    /// A `match` block
    ///
    /// `match <expr> with | <pattern> => <code>`
    Match {
        expr: Box<Expr>,
        arms: Vec<MatchArm>,
        position: Position,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Expr::Literal { value, .. } => value.to_string(),
                Expr::Tuple { values, .. } => format!("{:?}", values),
                Expr::Array { values, .. } => format!("{:?}", values),
                Expr::Grouping { expr, .. } => expr.to_string(),
                Expr::Unary { op, rhs, .. } => format!("({}{})", op, rhs),
                Expr::Binary { op, lhs, rhs, .. } => format!("({} {} {})", op, lhs, rhs),
                Expr::Call { name, args, .. } => format!("{}({:?})", name, args),
                Expr::Assignment { name, value, .. } => format!("{} = {}", name, value),
                Expr::Block { code, .. } => format!("do {:?} end", code),
                Expr::If {
                    condition,
                    body,
                    _else,
                    ..
                } if _else.is_none() => format!("if {} do {:?} end", condition, body),
                Expr::If {
                    condition,
                    body,
                    _else,
                    ..
                } => format!("if {} do {:?} else {:?} end", condition, body, _else),
                Expr::For {
                    var, iter, body, ..
                } => format!("for {} in {} do {:?} end", var, iter, body),
                Expr::While { expr, body, .. } => format!("while {} do {:?} end", expr, body),
                Expr::Match { expr, arms, .. } => format!("match {} with {:?}", expr, arms),
            }
        )
    }
}
