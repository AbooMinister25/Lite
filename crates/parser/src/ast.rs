//! The AST (Abstract Syntax Tree) represents Lite's grammar and syntax through
//! a tree-like representation.

use span::Spanned;
use std::fmt;

/// Kinds of literals
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
    /// Character literal (`'c'`)
    Char(char),
}

/// Binary operators
#[derive(Debug, Clone, PartialEq, Eq)]
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

/// Unary operators
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpKind {
    /// `!` operator
    Not,
    /// `-` operator
    Neg,
}

/// A type annotation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Annotation {
    Single(String),
    Tuple(Vec<Annotation>),
    Array(Vec<Annotation>),
}

/// A statement is a standalone unit of code comprised of one or more statements.
#[derive(Debug, Clone, PartialEq)]
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
}

/// An expression is an item which evaluates to some value
#[derive(Debug, Clone, PartialEq)]
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
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Int(i) => format!("(Int {i})"),
                Self::Float(f) => format!("(Float {f})"),
                Self::Bool(b) => format!("(Bool {b})"),
                Self::String(s) => format!("(String {s})"),
                Self::Char(c) => format!("(Char {c})"),
            }
        )
    }
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

impl fmt::Display for Annotation {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Single(s) => s.to_string(),
                Self::Tuple(t) => format!("{t:?}"),
                Self::Array(a) => format!("{a:?}"),
            }
        )
    }
}

impl fmt::Display for Statement {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Expression(e) => format!("(Expression\n  {}\n)", e.0),
                Self::Return(e) => format!("(Return {})", e.0),
                Self::Import(e) => format!("(Import {})", e.0),
                Self::Let {
                    name,
                    value,
                    mutable,
                } if *mutable => format!(
                    "Let(\n    name {}\n    mutable {mutable}\n    value {}\n)",
                    name.0, value.0
                ),
                Self::Let { name, value, .. } => format!("let {} = {}", name.0, value.0),
                Self::Function {
                    name,
                    public: _,
                    params,
                    annotations: _,
                    return_annotation: _,
                    body,
                } => format!("func {}({params:?}) do {}", name.0, body.0),
            }
        )
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Self::Literal(e) => e.to_string(),
                Self::Ident(i) => format!("(Ident {i})"),
                Self::Tuple(v) => format!(
                    "(Tuple {})",
                    v.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Self::Array(v) => format!(
                    "(Array {})",
                    v.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Self::Unary { op, rhs } => format!("(Unary ({op}{}))", rhs.0),
                Self::Binary { op, lhs, rhs } => format!("(Binary ({} {op} {}))", lhs.0, rhs.0),
                Self::Call { callee, args } => format!(
                    "(Call {}({}) )",
                    callee.0,
                    args.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Self::Assignment { name, op, value } =>
                    format!("(Assignment {} {op} {})", name.0, value.0),
                Self::Block(c) => format!(
                    "(Block {})",
                    c.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Self::If {
                    condition,
                    body,
                    else_,
                } if else_.is_none() =>
                    format!("(If\n  condition {}\n  body {}\n)", condition.0, body.0),
                Self::If {
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
                Self::For { var, iter, body } =>
                    format!("For[var: {} iter: {} body: {}]", var.0, iter.0, body.0),
                Self::While { expr, body } => format!("While[expr: {} body: {}]", expr.0, body.0),
            }
        )
    }
}
