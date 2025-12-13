use crate::lex::Span;

#[derive(Debug)]
pub struct Program {
    pub imports: Vec<Import>,
    pub structs: Vec<StructDef>,
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct LinkedProgram {
    pub structs: Vec<StructDef>,
    pub funcs: Vec<Func>,
}

#[derive(Debug)]
pub struct Import {
    pub path: String,
    pub span: Span,
}

#[derive(Debug)]
pub struct StructDef {
    pub name: String,
    pub fields: Vec<Param>,
    pub span: Span,
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
    }, // vars/lock
    Assign {
        target: AssignTarget,
        expr: Expr,
        span: Span,
    }, // :=
    If {
        arms: Vec<(Expr, Vec<Stmt>)>,
        else_body: Option<Vec<Stmt>>,
        #[allow(dead_code)]
        span: Span,
    },
    While {
        cond: Expr,
        body: Vec<Stmt>,
        #[allow(dead_code)]
        span: Span,
    }, // loop <cond>:
    Break {
        span: Span,
    }, // stop/brk
    Continue {
        span: Span,
    }, // next/cnt
    Back {
        expr: Option<Expr>,
        span: Span,
    },
    Expr {
        expr: Expr,
        span: Span,
    }, // 例: echo(...), foo(...)
}

#[derive(Debug)]
pub enum AssignTarget {
    Name(String),
    Field { base: String, field: String },
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
    BuiltinPrint(Span), // echo / prn
    StructLit {
        name: String,
        fields: Vec<(String, Expr, Span)>,
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
}

impl Expr {
    pub fn span(&self) -> Span {
        match self {
            Expr::Int(_, s) => *s,
            Expr::Str(_, s) => *s,
            Expr::Ident(_, s) => *s,
            Expr::BuiltinPrint(s) => *s,
            Expr::StructLit { span, .. } => *span,
            Expr::Field { span, .. } => *span,
            Expr::Call { span, .. } => *span,
            Expr::Unary { span, .. } => *span,
            Expr::Binary { span, .. } => *span,
        }
    }
}
