use crate::lex::Span;

#[derive(Debug)]
pub struct Program {
    pub imports: Vec<Import>,
    pub enums: Vec<EnumDef>,
    pub structs: Vec<StructDef>,
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct LinkedProgram {
    pub enums: Vec<EnumDef>,
    pub structs: Vec<StructDef>,
    pub funcs: Vec<Func>,
    pub source_map: Vec<std::path::PathBuf>,
}

#[derive(Debug)]
pub struct Import {
    pub path: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct EnumDef {
    pub name: String,
    pub variants: Vec<EnumVariant>,
    pub span: Span,
}

#[derive(Debug)]
pub struct EnumVariant {
    pub name: String,
    pub fields: Vec<(String, Span)>,
    pub span: Span,
}

#[derive(Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Param>,
    pub span: Span,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Visibility {
    Public,
    Private,
}

#[derive(Debug)]
pub struct Func {
    pub method: Option<MethodInfo>,
    pub name: String,
    pub params: Vec<Param>,
    pub ret_ty: Option<String>, // None => void (but main is required in sem)
    pub body: Vec<Stmt>,
    pub span: Span,
}

#[derive(Debug)]
pub struct MethodInfo {
    pub type_name: String,
    pub method_name: String,
    pub has_self: bool,
}

#[derive(Debug)]
pub struct Param {
    pub name: String,
    pub ty: String,
    pub mutable: bool,
    pub vis: Visibility,
    pub span: Span,
}

#[derive(Debug)]
pub enum Stmt {
    VarDecl {
        mutable: bool,
        name: String,
        ty: String,
        init: Expr,
        span: Span,
    }, // cell/bind
    Assign {
        target: AssignTarget,
        expr: Expr,
        span: Span,
    }, // :=
    If {
        arms: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
        span: Span,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
        span: Span,
    }, // loop <cond>:
    Foreach {
        name: String, // loop variable name
        ty: String,   // loop variable type
        iter: Expr,   // iterator object expression
        body: Vec<Stmt>,
        span: Span,
    },
    Case {
        scrutinee: Expr,
        arms: Vec<(Pattern, Vec<Stmt>)>,
        span: Span,
    },
    Break {
        span: Span,
    }, // stop/brk
    Continue {
        span: Span,
    }, // next/nxt
    Back {
        expr: Option<Expr>,
        span: Span,
    },
    Expr {
        expr: Expr,
        span: Span,
    }, // e.g. println(...), foo(...)
}

#[derive(Debug)]
pub enum AssignTarget {
    Name(String),
    Field { base: String, field: String },
    Index { base: String, index: Expr },
}

#[derive(Debug)]
pub enum Arg {
    Pos(Expr),
    Named {
        name: String,
        expr: Expr,
        span: Span,
    },
}

#[derive(Debug)]
pub enum Pattern {
    EnumVariant {
        enum_name: String,
        variant: String,
        bindings: Vec<String>,
        span: Span,
    },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnOp {
    Neg,
    Not,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinOp {
    Mul,
    Div,
    Mod,
    Add,
    Sub,
    Shl,
    Shr,
    BitAnd,
    BitXor,
    BitOr,
    Lt,
    Le,
    Gt,
    Ge,
    Eq,
    Ne, // non-assoc
    And,
    Or,
}

#[derive(Debug)]
pub enum Expr {
    Int(i64, Span),
    Str(String, Span),
    Ident(String, Span),
    ArrayLit {
        elements: Vec<Expr>,
        span: Span,
    },
    EnumLit {
        enum_name: String,
        variant: String,
        args: Vec<Expr>,
        span: Span,
    },
    Index {
        base: Box<Expr>,
        index: Box<Expr>,
        span: Span,
    },
    Field {
        base: Box<Expr>,
        field: String,
        span: Span,
    },
    Call {
        callee: Box<Expr>,
        args: Vec<Arg>,
        span: Span,
    },
    Unary {
        op: UnOp,
        expr: Box<Expr>,
        span: Span,
    },
    Binary {
        op: BinOp,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
        span: Span,
    },

    Try {
        expr: Box<Expr>,
        span: Span,
    },
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, s) => *s,
            Expr::Str(_, s) => *s,
            Expr::Ident(_, s) => *s,
            Expr::ArrayLit { span, .. } => *span,
            Expr::EnumLit { span, .. } => *span,
            Expr::Index { span, .. } => *span,
            Expr::Field { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Binary { span, .. } => *span,
            Expr::Try { span, .. } => *span,
        }
    }
}
