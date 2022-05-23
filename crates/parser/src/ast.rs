//! The `AST` (Abstract Syntax Tree) makes up a tree like
//! representation of Lite's syntax. This file contains structures
//! to represent different parts of the AST. The most notable are the
//! `Expr` and `Statement` enums.

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
    /// Character literal (`'c'`)
    Char(char),
}

impl fmt::Display for LiteralKind {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                LiteralKind::Int(i) => format!("Int[{}]", i),
                LiteralKind::Float(f) => format!("Float[{}]", f),
                LiteralKind::Bool(b) => format!("Bool[{}]", b),
                LiteralKind::String(s) => format!("String[{}]", s),
                LiteralKind::Char(c) => format!("Char[{}]", c),
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

pub type Spanned<T> = (T, std::ops::Range<usize>);

/// A statement is some standalone unit which does something comprised of
/// one or more statements.
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
        name: Spanned<String>,
        value: Spanned<Expr>,
        mutable: bool,
    },
    /// A function declaration
    ///
    /// `func <name>(<args>) do <expr> end`
    Function {
        name: Spanned<String>,
        params: Spanned<Vec<Spanned<String>>>,
        annotations: Vec<Spanned<Annotation>>,
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
                Statement::Expression(e) => e.0.to_string(),
                Statement::Return(e) => format!("return {}", e.0),
                Statement::Import(e) => format!("return {}", e.0),
                Statement::Let {
                    name,
                    value,
                    mutable,
                } if *mutable => format!("let mut {} = {}", name.0, value.0),
                Statement::Let { name, value, .. } => format!("let {} = {}", name.0, value.0),
                Statement::Function {
                    name,
                    params,
                    annotations: _,
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
    Block(Vec<Spanned<Expr>>),
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
                Expr::Ident(i) => format!("Ident[{}]", i.to_string()),
                Expr::Tuple(v) => format!(
                    "Tuple[{}]",
                    v.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Expr::Array(v) => format!(
                    "Array[{}]",
                    v.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Expr::Unary { op, rhs } => format!("Unary[({}{})]", op, rhs.0),
                Expr::Binary { op, lhs, rhs } => format!("Binary[({} {} {})]", lhs.0, op, rhs.0),
                Expr::Call { callee, args } => format!("Call[{}({:?})]", callee.0, args),
                Expr::Assignment { name, op, value } =>
                    format!("Assignment[{} {} {}]", name.0, op, value.0),
                Expr::Block(c) => format!(
                    "Block[{}]",
                    c.iter()
                        .map(|i| i.0.to_string())
                        .collect::<Vec<String>>()
                        .join(", ")
                ),
                Expr::If {
                    condition,
                    body,
                    else_,
                } if else_.is_none() => format!("If[{} -> {}]", condition.0, body.0),
                Expr::If {
                    condition,
                    body,
                    else_,
                } => format!(
                    "If[{} -> {} ^ ~{} -> {:?}]",
                    condition.0,
                    body.0,
                    condition.0,
                    else_.clone().unwrap().0
                ),
                Expr::For {
                    var, iter, body, ..
                } => format!("for {} in {} do {:?} end", var.0, iter.0, body),
                Expr::While { expr, body, .. } => format!("while {} do {:?} end", expr.0, body),
                Expr::Match { expr, arms, .. } => format!("match {} with {:?}", expr.0, arms),
            }
        )
    }
}
