//! The `AST` (Abstract Syntax Tree) makes up a tree like
//! representation of Lite's syntax. This file contains structures
//! to represent different parts of the AST. The most notable are the
//! `Expr` and `Statement` enums.

use serde::Serialize;
use span::Spanned;
use std::fmt;

/// Kinds of Literals
///
/// integers, floats, booleans, strings, identifiers, and chars.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum LiteralKind {
    /// Integer literal (`10`)
    Int(i64),
    /// Float literal (`3.5`)
    Float(f64),
    /// Boolean literal (`true`, `false`)
    Bool(bool),
    /// String literal (`"foo"`)
    String(String),
    /// Character literal (`'c'`)
    Char(char),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LiteralKind::Int(i) => format!("(Int {})", i),
                LiteralKind::Float(f) => format!("(Float {})", f),
                LiteralKind::Bool(b) => format!("(Bool {})", b),
                LiteralKind::String(s) => format!("(String {})", s),
                LiteralKind::Char(c) => format!("(Char {})", c),
            }
        )
    }
}

/// Binary operators
#[derive(Debug, Clone, PartialEq, Serialize)]
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
                Self::Add => "+",
                Self::Sub => "-",
                Self::Mul => "*",
                Self::Div => "/",
                Self::And => "and",
                Self::Or => "or",
                Self::Eq => "==",
                Self::Ne => "!=",
                Self::Gt => ">",
                Self::Ge => ">=",
                Self::Lt => "<",
                Self::Le => "<=",
            }
        )
    }
}

/// Unary operators
#[derive(Debug, Clone, PartialEq, Serialize)]
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
                Self::Not => "!",
                Self::Neg => "-",
            }
        )
    }
}

/// A range
///
/// `1..10`, `3..=9`
#[derive(Debug, Clone, PartialEq, Serialize)]
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
#[derive(Debug, Clone, PartialEq, Serialize)]
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
                Self::Wild => "_".to_string(),
                Self::Ident(i) => i.to_string(),
                Self::Literal(l) => l.to_string(),
                Self::Or(o) => format!("{:?}", o),
                Self::Range(r) => r.to_string(),
                Self::Tuple(t) => format!("{:?}", t),
                Self::Rest => "..".to_string(),
            }
        )
    }
}

#[derive(Debug, Clone, PartialEq, Serialize)]
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

#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Annotation {
    Single(String),
    Tuple(Vec<Annotation>),
    Array(Vec<Annotation>),
}

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Annotation::Single(s) => s.to_string(),
                Annotation::Tuple(t) => format!("{:?}", t),
                Annotation::Array(a) => format!("{:?}", a),
            }
        )
    }
}

/// A statement is some standalone unit which does something comprised of
/// one or more statements.
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Statement {
    /// An expression statement
    Expression(Spanned<Expr>),
    /// A return statement
    ///
    /// `return <expr>`
    Return(Spanned<Expr>),
    /// An import statement
    ///
    /// `import package`
    Import(Spanned<Expr>),
    /// A `let` variable declaration
    ///
    /// `let [mut] <name> = <expr>`
    Let {
        name: Spanned<Expr>,
        value: Spanned<Expr>,
        mutable: bool,
    },
    /// A function declaration
    ///
    /// `func <name>(<args>) do <expr> end`
    Function {
        name: Spanned<Expr>,
        public: bool,
        params: Vec<String>,
        annotations: Vec<Spanned<Annotation>>,
        return_annotation: Option<Spanned<Annotation>>,
        body: Spanned<Expr>,
    },
    /// A class declaration
    ///
    /// `class <name> [is] [traits] [of] [superclass] do [fields] [methods] end`
    Class {
        name: Spanned<String>,
        superclass: Option<Spanned<Expr>>,
        traits: Vec<Spanned<Expr>>,
        fields: Vec<Spanned<Expr>>,
        instance_methods: Vec<Spanned<Statement>>,
        class_methods: Vec<Spanned<Statement>>,
    },
    /// A trait declaration
    ///
    /// `trait <name> do [fields] [methods] end`
    Trait {
        name: Spanned<String>,
        attributes: Vec<Spanned<Expr>>,
        methods: Vec<Spanned<Statement>>,
    },
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Statement::Expression(e) => format!("(Expression\n  {}\n)", e.0),
                Statement::Return(e) => format!("(Return {})", e.0),
                Statement::Import(e) => format!("(Import {})", e.0),
                Statement::Let {
                    name,
                    value,
                    mutable,
                } if *mutable => format!("Let(\n    name {}\n    mutable {}\n    value {}\n)", name.0, mutable, value.0),
                Statement::Let { name, value, .. } => format!("let {} = {}", name.0, value.0),
                Statement::Function {
                    name,
                    public,
                    params,
                    annotations,
                    return_annotation,
                    body,
                } => format!("func {}({:?}) do {}", name.0, params, body.0),
                Statement::Class {
                    name,
                    superclass,
                    traits,
                    fields,
                    instance_methods,
                    class_methods,
                } => format!(
                    "Class[name: {}, traits: {:?}, superclass: {:?}, fields: {:?}, instance_methods: {:?}, class_methods: {:?}",
                    name.0,
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
                } => format!("Trait[name: {}, attributes: {:?}, methods: {:?}", name.0, attributes, methods),
            }
        )
    }
}

/// An expression is some item that evaluates to some value
#[derive(Debug, Clone, PartialEq, Serialize)]
pub enum Expr {
    /// Literals (`10`, `"Hi"`)
    Literal(LiteralKind),
    /// Identifiers (`hello`, `foo`, `bar`)
    Ident(String),
    /// Tuples (`(1, 2, 3)`)
    Tuple(Vec<Spanned<Expr>>),
    /// Arrays (`[1, 2, 3]`)
    Array(Vec<Spanned<Expr>>),
    /// An unary operation (`!foo`, `-bar`)
    Unary { op: String, rhs: Box<Spanned<Expr>> },
    /// A binary operation (`5 + 5`)
    Binary {
        op: String,
        lhs: Box<Spanned<Expr>>,
        rhs: Box<Spanned<Expr>>,
    },
    /// A function call (`foo()`)
    Call {
        callee: Box<Spanned<Expr>>,
        args: Vec<Spanned<Expr>>,
    },
    /// A variable assignment (`foo = 10`)
    Assignment {
        name: Box<Spanned<Expr>>,
        op: String,
        value: Box<Spanned<Expr>>,
    },
    /// A block
    ///
    /// `do <code> end`
    Block(Vec<Spanned<Statement>>),
    /// An `if` expression
    ///
    /// `if <expr> do <code> else <code> end`
    If {
        condition: Box<Spanned<Expr>>,
        body: Box<Spanned<Expr>>,
        else_: Box<Option<Spanned<Expr>>>,
    },
    /// A for loop
    ///
    /// `for i in it do <code> end`
    For {
        var: Box<Spanned<Expr>>,
        iter: Box<Spanned<Expr>>,
        body: Box<Spanned<Expr>>,
    },
    /// A while loop
    ///
    /// `while <expr> do <code> end`
    While {
        expr: Box<Spanned<Expr>>,
        body: Box<Spanned<Expr>>,
    },
    /// A `match` block
    ///
    /// `match <expr> with | <pattern> => <code>`
    Match {
        expr: Box<Spanned<Expr>>,
        arms: Vec<Spanned<Expr>>,
    },
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
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
                Expr::Assignment { name, op, value } =>
                    format!("(Assignment {} {} {})", name.0, op, value.0),
                Expr::Block(c) => format!(
                    "(Block {})",
                    c.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Expr::If {
                    condition,
                    body,
                    else_,
                } if else_.is_none() =>
                    format!("(If\n  condition {}\n  body {}\n)", condition.0, body.0),
                Expr::If {
                    condition,
                    body,
                    else_,
                } => format!(
                    "If[{} -> {} ^ ~{} -> {}]",
                    condition.0,
                    body.0,
                    condition.0,
                    else_.clone().unwrap().0
                ),
                Expr::For { var, iter, body } =>
                    format!("For[var: {} iter: {} body: {}]", var.0, iter.0, body.0),
                Expr::While { expr, body } => format!("While[expr: {} body: {}]", expr.0, body.0),
                Expr::Match { expr, arms } => format!("match {} with {:?}", expr.0, arms),
            }
        )
    }
}
