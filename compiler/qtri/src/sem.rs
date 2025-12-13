use crate::ast::*;
use crate::lex::Span;
use std::collections::{HashMap, HashSet};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Ty {
    Int,
    Bool,
    Text,
    Void,
    Struct(String),
}

#[derive(Debug)]
pub struct FnSig {
    pub ret: Ty,
    pub params: Vec<(String, Ty)>,
}

#[derive(Debug)]
pub struct MethodSig {
    pub ret: Ty,
    pub params: Vec<(String, Ty)>,
    pub has_self: bool,
    pub symbol: String,
}

#[derive(Debug, Clone)]
pub struct StructInfo {
    pub fields: Vec<(String, Ty)>,
}

#[derive(Debug)]
pub struct SemInfo {
    pub fns: HashMap<String, FnSig>,
    pub structs: HashMap<String, StructInfo>,
    pub methods: HashMap<String, HashMap<String, MethodSig>>, // type -> method -> sig
}

#[derive(Debug)]
pub struct SemError {
    pub span: Span,
    pub msg: String,
}
impl std::fmt::Display for SemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.line, self.span.col, self.msg)
    }
}
impl std::error::Error for SemError {}
fn serr(span: Span, msg: impl Into<String>) -> SemError {
    SemError {
        span,
        msg: msg.into(),
    }
}

pub fn check(prog: &LinkedProgram) -> Result<SemInfo, SemError> {
    let mut fns: HashMap<String, FnSig> = HashMap::new();
    let mut structs: HashMap<String, StructInfo> = HashMap::new();
    let mut methods: HashMap<String, HashMap<String, MethodSig>> = HashMap::new();

    for s in &prog.structs {
        if structs.contains_key(&s.name) {
            return Err(serr(
                s.span,
                format!("duplicate struct type: {}", s.name),
            ));
        }

        let mut fields = Vec::new();
        let mut seen = HashMap::<String, Span>::new();
        for f in &s.fields {
            if let Some(prev) = seen.insert(f.name.clone(), f.span) {
                return Err(serr(
                    f.span,
                    format!(
                        "duplicate field: {} (previous at {}:{})",
                        f.name, prev.line, prev.col
                    ),
                ));
            }
            let fty = parse_ty(&f.ty, &structs)
                .ok_or_else(|| serr(f.span, format!("unknown type: {}", f.ty)))?;
            if matches!(fty, Ty::Struct(_)) {
                return Err(serr(
                    f.span,
                    "struct fields of struct type are not supported yet",
                ));
            }
            fields.push((f.name.clone(), fty));
        }
        structs.insert(s.name.clone(), StructInfo { fields });
    }

    for f in &prog.funcs {
        if fns.contains_key(&f.name) {
            return Err(serr(f.span, format!("duplicate function: {}", f.name)));
        }
        let ret = match &f.ret_ty {
            None => Ty::Void,
            Some(s) => parse_ty(s, &structs)
                .ok_or_else(|| serr(f.span, format!("unknown type: {s}")))?,
        };
        let mut params = Vec::new();
        let mut seen = HashMap::<String, Span>::new();
        for p in &f.params {
            if let Some(prev) = seen.insert(p.name.clone(), p.span) {
                return Err(serr(
                    p.span,
                    format!(
                        "duplicate param: {} (previous at {}:{})",
                        p.name, prev.line, prev.col
                    ),
                ));
            }
            let t = parse_ty(&p.ty, &structs)
                .ok_or_else(|| serr(p.span, format!("unknown type: {}", p.ty)))?;
            params.push((p.name.clone(), t));
        }

        if let Some(method) = &f.method {
            if method.has_self {
                match params.first() {
                    Some((_, Ty::Struct(ref s))) if s == &method.type_name => {}
                    Some((_, other)) => {
                        return Err(serr(
                            f.span,
                            format!("self must be of type {} (got {:?})", method.type_name, other),
                        ))
                    }
                    None => {
                        return Err(serr(f.span, "method marked as having self but no params"));
                    }
                }
            }
            let entry = methods.entry(method.type_name.clone()).or_default();
            if entry.contains_key(&method.method_name) {
                return Err(serr(
                    f.span,
                    format!(
                        "duplicate method {} for {}",
                        method.method_name, method.type_name
                    ),
                ));
            }
            entry.insert(
                method.method_name.clone(),
                MethodSig {
                    ret: ret.clone(),
                    params: params.clone(),
                    has_self: method.has_self,
                    symbol: f.name.clone(),
                },
            );
        }

        fns.insert(f.name.clone(), FnSig { ret, params });
    }

    // main must exist and be int
    let main_sig = fns
        .get("main")
        .ok_or_else(|| serr(Span { line: 1, col: 1 }, "missing function: main"))?;
    if main_sig.ret != Ty::Int {
        return Err(serr(
            Span { line: 1, col: 1 },
            "main must return intg/int (i64)",
        ));
    }

    // per-function check
    for f in &prog.funcs {
        let sig = fns.get(&f.name).unwrap();
        check_func(f, sig, &fns, &structs, &methods)?;
    }

    Ok(SemInfo {
        fns,
        structs,
        methods,
    })
}

fn parse_ty(s: &str, structs: &HashMap<String, StructInfo>) -> Option<Ty> {
    Some(match s {
        "intg" | "int" => Ty::Int,
        "bool" | "bol" => Ty::Bool,
        "text" | "txt" => Ty::Text,
        "void" | "vod" => Ty::Void,
        other => {
            if structs.contains_key(other) {
                Ty::Struct(other.to_string())
            } else {
                return None;
            }
        }
    })
}

#[derive(Clone)]
struct VarInfo {
    ty: Ty,
    mutable: bool,
}

fn check_func(
    f: &Func,
    sig: &FnSig,
    fns: &HashMap<String, FnSig>,
    structs: &HashMap<String, StructInfo>,
    methods: &HashMap<String, HashMap<String, MethodSig>>,
) -> Result<(), SemError> {
    let mut scopes: Vec<HashMap<String, VarInfo>> = vec![HashMap::new()];

    // params as immutable vars
    for (i, (name, ty)) in sig.params.iter().enumerate() {
        let mutability = f
            .method
            .as_ref()
            .map(|m| m.has_self && i == 0)
            .unwrap_or(false)
            || f.params.get(i).map(|p| p.mutable).unwrap_or(false);
        scopes[0].insert(
            name.clone(),
            VarInfo {
                ty: ty.clone(),
                mutable: mutability,
            },
        );
    }

    let mut loop_depth = 0usize;
    let mut saw_return = false;

    for st in &f.body {
        saw_return |=
            check_stmt(st, &sig.ret, fns, methods, structs, &mut scopes, &mut loop_depth)?;
    }

    // main must have explicit back <expr>
    if f.name == "main" && !saw_return {
        return Err(serr(f.span, "main must have `back <expr>`"));
    }
    // non-void must return somewhere
    if sig.ret != Ty::Void && !saw_return {
        return Err(serr(f.span, "non-void function must have `back <expr>`"));
    }

    Ok(())
}

fn check_stmt(
    st: &Stmt,
    ret_ty: &Ty,
    fns: &HashMap<String, FnSig>,
    methods: &HashMap<String, HashMap<String, MethodSig>>,
    structs: &HashMap<String, StructInfo>,
    scopes: &mut Vec<HashMap<String, VarInfo>>,
    loop_depth: &mut usize,
) -> Result<bool, SemError> {
    match st {
        Stmt::VarDecl {
            mutable,
            name,
            ty,
            init,
            span,
        } => {
            let decl_ty =
                parse_ty(ty, structs).ok_or_else(|| serr(*span, format!("unknown type: {ty}")))?;
            let init_ty = check_expr(init, fns, methods, scopes, structs)?;
            if decl_ty != init_ty {
                return Err(serr(
                    *span,
                    format!(
                        "type mismatch: {} := {:?} (init is {:?})",
                        name, decl_ty, init_ty
                    ),
                ));
            }
            scopes.last_mut().unwrap().insert(
                name.clone(),
                VarInfo {
                    ty: decl_ty,
                    mutable: *mutable,
                },
            );
            Ok(false)
        }
        Stmt::Assign { target, expr, span } => match target {
            AssignTarget::Name(name) => {
                let (vty, mutability) = lookup_var(name, *span, scopes)?;
                if !mutability {
                    return Err(serr(*span, format!("cannot assign to lock: {name}")));
                }
                let ety = check_expr(expr, fns, methods, scopes, structs)?;
                if vty != ety {
                    return Err(serr(
                        *span,
                        format!("type mismatch in assignment to {name}"),
                    ));
                }
                Ok(false)
            }
            AssignTarget::Field { base, field } => {
                let (bty, mutability) = lookup_var(base, *span, scopes)?;
                if !mutability {
                    return Err(serr(*span, format!("cannot assign to lock: {base}")));
                }
                let sname = match bty {
                    Ty::Struct(ref n) => n,
                    _ => {
                        return Err(serr(
                            *span,
                            format!("{base} is not a struct for field assignment"),
                        ))
                    }
                };
                let sinfo = structs.get(sname).ok_or_else(|| {
                    serr(*span, format!("unknown struct during assignment: {sname}"))
                })?;
                let mut fty = None;
                for (fname, fty_val) in &sinfo.fields {
                    if fname == field {
                        fty = Some(fty_val.clone());
                        break;
                    }
                }
                let fty = fty.ok_or_else(|| serr(*span, format!("unknown field: {field}")))?;
                let ety = check_expr(expr, fns, methods, scopes, structs)?;
                if fty != ety {
                    return Err(serr(*span, "type mismatch in field assignment"));
                }
                Ok(false)
            }
        },
        Stmt::Expr { expr, span } => {
            // MVP: statement expression must be a call
            match expr {
                Expr::Call { .. } => {
                    let _ = check_expr(expr, fns, methods, scopes, structs)?;
                    Ok(false)
                }
                _ => Err(serr(
                    *span,
                    "only function calls are allowed as expression statements",
                )),
            }
        }
        Stmt::Back { expr, span } => {
            match ret_ty {
                Ty::Void => {
                    if expr.is_some() {
                        return Err(serr(*span, "void function cannot return a value"));
                    }
                }
                _ => {
                    let e = expr
                        .as_ref()
                        .ok_or_else(|| serr(*span, "non-void function must return a value"))?;
                    let ety = check_expr(e, fns, methods, scopes, structs)?;
                    if ety != *ret_ty {
                        return Err(serr(*span, "return type mismatch"));
                    }
                }
            }
            Ok(true)
        }
        Stmt::If {
            arms, else_body, ..
        } => {
            for (c, b) in arms {
                let cty = check_expr(c, fns, methods, scopes, structs)?;
                if cty != Ty::Bool {
                    return Err(serr(c.span(), "when/elif condition must be bool"));
                }
                scopes.push(HashMap::new());
                let mut local_ret = false;
                for s in b {
                    local_ret |= check_stmt(s, ret_ty, fns, methods, structs, scopes, loop_depth)?;
                }
                scopes.pop();
                let _ = local_ret;
            }
            if let Some(b) = else_body {
                scopes.push(HashMap::new());
                let mut local_ret = false;
                for s in b {
                    local_ret |= check_stmt(s, ret_ty, fns, methods, structs, scopes, loop_depth)?;
                }
                scopes.pop();
                let _ = local_ret;
            }
            Ok(false)
        }
        Stmt::While { cond, body, .. } => {
            let cty = check_expr(cond, fns, methods, scopes, structs)?;
            if cty != Ty::Bool {
                return Err(serr(cond.span(), "loop condition must be bool"));
            }
            *loop_depth += 1;
            scopes.push(HashMap::new());
            let mut local_ret = false;
            for s in body {
                local_ret |= check_stmt(s, ret_ty, fns, methods, structs, scopes, loop_depth)?;
            }
            scopes.pop();
            *loop_depth -= 1;
            let _ = local_ret;
            Ok(false)
        }
        Stmt::Break { span } => {
            if *loop_depth == 0 {
                return Err(serr(*span, "stop/brk is only allowed inside loop"));
            }
            Ok(true)
        }
        Stmt::Continue { span } => {
            if *loop_depth == 0 {
                return Err(serr(*span, "next/cnt is only allowed inside loop"));
            }
            Ok(true)
        }
    }
}

fn lookup_var(
    name: &str,
    sp: Span,
    scopes: &Vec<HashMap<String, VarInfo>>,
) -> Result<(Ty, bool), SemError> {
    for scope in scopes.iter().rev() {
        if let Some(v) = scope.get(name) {
            return Ok((v.ty.clone(), v.mutable));
        }
    }
    Err(serr(sp, format!("unknown variable: {name}")))
}

fn check_expr(
    e: &Expr,
    fns: &HashMap<String, FnSig>,
    methods: &HashMap<String, HashMap<String, MethodSig>>,
    scopes: &Vec<HashMap<String, VarInfo>>,
    structs: &HashMap<String, StructInfo>,
) -> Result<Ty, SemError> {
    match e {
        Expr::Int(_, _) => Ok(Ty::Int),
        Expr::Str(_, _) => Ok(Ty::Text),
        Expr::Ident(name, sp) => {
            for scope in scopes.iter().rev() {
                if let Some(v) = scope.get(name) {
                    return Ok(v.ty.clone());
                }
            }
            Err(serr(*sp, format!("unknown identifier: {name}")))
        }
        Expr::StructLit { name, fields, span } => {
            let sinfo = structs
                .get(name)
                .ok_or_else(|| serr(*span, format!("unknown struct: {name}")))?;
            let mut seen = HashSet::new();
            for (fname, expr, fspan) in fields {
                let fty = sinfo
                    .fields
                    .iter()
                    .find(|(n, _)| n == fname)
                    .ok_or_else(|| serr(*fspan, format!("unknown field: {fname}")))?
                    .1
                    .clone();
                let ety = check_expr(expr, fns, methods, scopes, structs)?;
                if ety != fty {
                    return Err(serr(*fspan, "field type mismatch"));
                }
                if !seen.insert(fname) {
                    return Err(serr(*fspan, format!("duplicate field: {fname}")));
                }
            }
            if seen.len() != sinfo.fields.len() {
                return Err(serr(*span, "missing field in struct literal"));
            }
            Ok(Ty::Struct(name.clone()))
        }
        Expr::Field { base, field, span } => {
            let bty = check_expr(base, fns, methods, scopes, structs)?;
            let sname = match bty {
                Ty::Struct(n) => n,
                _ => return Err(serr(*span, "field access on non-struct")),
            };
            let sinfo = structs
                .get(&sname)
                .ok_or_else(|| serr(*span, format!("unknown struct: {sname}")))?;
            let fty = sinfo
                .fields
                .iter()
                .find(|(n, _)| n == field)
                .ok_or_else(|| serr(*span, format!("unknown field: {field}")))?
                .1
                .clone();
            Ok(fty)
        }
        Expr::BuiltinPrint(_) => Ok(Ty::Void),
        Expr::Unary { op, expr, span } => {
            let t = check_expr(expr, fns, methods, scopes, structs)?;
            match op {
                UnOp::Neg if t == Ty::Int => Ok(Ty::Int),
                UnOp::Not if t == Ty::Bool => Ok(Ty::Bool),
                _ => Err(serr(*span, "invalid unary op/type")),
            }
        }
        Expr::Binary { op, lhs, rhs, span } => {
            let a = check_expr(lhs, fns, methods, scopes, structs)?;
            let b = check_expr(rhs, fns, methods, scopes, structs)?;
            use BinOp::*;
            let r = match op {
                Add | Sub | Mul | Div | Mod | Shl | Shr | BitAnd | BitXor | BitOr => {
                    if a == Ty::Int && b == Ty::Int {
                        Ty::Int
                    } else {
                        return Err(serr(*span, "int op requires int"));
                    }
                }
                Lt | Le | Gt | Ge | Eq | Ne => {
                    if a == Ty::Int && b == Ty::Int {
                        Ty::Bool
                    } else {
                        return Err(serr(*span, "cmp requires int"));
                    }
                }
                And | Or => {
                    if a == Ty::Bool && b == Ty::Bool {
                        Ty::Bool
                    } else {
                        return Err(serr(*span, "logic requires bool"));
                    }
                }
            };
            Ok(r)
        }
        Expr::Call { callee, args, span } => {
            // builtin echo(...)
            if matches!(&**callee, Expr::BuiltinPrint(_)) {
                if args.len() != 1 {
                    return Err(serr(*span, "echo takes exactly 1 arg"));
                }
                match &args[0] {
                    Arg::Pos(x) => {
                        let t = check_expr(x, fns, methods, scopes, structs)?;
                        if t != Ty::Int && t != Ty::Text {
                            return Err(serr(x.span(), "echo arg must be int or text"));
                        }
                        Ok(Ty::Void)
                    }
                    Arg::Named { .. } => Err(serr(*span, "echo does not take named args")),
                }
            } else if let Expr::Field {
                base,
                field,
                span: field_span,
            } = &**callee
            {
                let validate_args = |expected: &[(String, Ty)]| -> Result<(), SemError> {
                    if args.iter().all(|a| matches!(a, Arg::Pos(_))) {
                        if args.len() != expected.len() {
                            return Err(serr(
                                *span,
                                format!(
                                    "arg count mismatch: expected {}, got {}",
                                    expected.len(),
                                    args.len()
                                ),
                            ));
                        }
                        for (i, a) in args.iter().enumerate() {
                            let expr = match a {
                                Arg::Pos(e) => e,
                                Arg::Named { .. } => unreachable!(),
                            };
                            let xt = check_expr(expr, fns, methods, scopes, structs)?;
                            if xt != expected[i].1 {
                                return Err(serr(expr.span(), "arg type mismatch"));
                            }
                        }
                        Ok(())
                    } else {
                        let mut named = HashMap::<String, (&Expr, Span)>::new();
                        for a in args {
                            match a {
                                Arg::Named { name, expr, span } => {
                                    if named.insert(name.clone(), (expr, *span)).is_some() {
                                        return Err(serr(*span, "duplicate named arg"));
                                    }
                                }
                                Arg::Pos(e) => {
                                    return Err(serr(e.span(), "cannot mix named and positional args"))
                                }
                            }
                        }
                        if named.len() != expected.len() {
                            return Err(serr(
                                *span,
                                format!(
                                    "argument count mismatch for named call: expected {}, got {}",
                                    expected.len(),
                                    named.len()
                                ),
                            ));
                        }
                        for (pname, pty) in expected {
                            let (expr, arg_sp) = named
                                .get(pname)
                                .ok_or_else(|| serr(*span, format!("missing parameter: {pname}")))?;
                            let ety = check_expr(expr, fns, methods, scopes, structs)?;
                            if ety != *pty {
                                return Err(serr(*arg_sp, "arg type mismatch"));
                            }
                        }
                        Ok(())
                    }
                };

                if let Expr::Ident(type_name, _) = &**base {
                    let table = methods
                        .get(type_name)
                        .ok_or_else(|| serr(*field_span, format!("unknown type: {type_name}")))?;
                    let sig = table
                        .get(field)
                        .ok_or_else(|| serr(*field_span, format!("unknown method: {field}")))?;
                    if sig.has_self {
                        return Err(serr(
                            *field_span,
                            "method expects self; call with value.method(...)",
                        ));
                    }
                    validate_args(&sig.params)?;
                    Ok(sig.ret.clone())
                } else {
                    let recv_ty = check_expr(base, fns, methods, scopes, structs)?;
                    let sname = match recv_ty {
                        Ty::Struct(n) => n,
                        _ => return Err(serr(*field_span, "method call requires struct receiver")),
                    };
                    let table = methods
                        .get(&sname)
                        .ok_or_else(|| serr(*field_span, format!("{sname} has no methods")))?;
                    let sig = table
                        .get(field)
                        .ok_or_else(|| serr(*field_span, format!("unknown method: {field}")))?;
                    if !sig.has_self {
                        return Err(serr(*field_span, "associated function cannot use value receiver"));
                    }
                    validate_args(&sig.params[1..])?;
                    Ok(sig.ret.clone())
                }
            } else if let Expr::Ident(fname, sp) = &**callee {
                let sig = fns
                    .get(fname)
                    .ok_or_else(|| serr(*sp, format!("unknown function: {fname}")))?;
                // args: positional only for now (named is parsed but we validate “no mixing” already)
                let mut pos = Vec::new();
                let mut named = HashMap::<String, (&Expr, Span)>::new();
                for a in args {
                    match a {
                        Arg::Pos(x) => pos.push(x),
                        Arg::Named { name, expr, span } => {
                            if named.insert(name.clone(), (expr, *span)).is_some() {
                                return Err(serr(*span, "duplicate named arg"));
                            }
                        }
                    }
                }
                if !named.is_empty() && !pos.is_empty() {
                    return Err(serr(*span, "cannot mix named and positional args"));
                }
                if !named.is_empty() {
                    // 1. Check for unknown parameter names.
                    let param_names: std::collections::HashSet<_> =
                        sig.params.iter().map(|p| &p.0).collect();
                    for (name, (_, arg_span)) in &named {
                        if !param_names.contains(name) {
                            return Err(serr(*arg_span, format!("unknown parameter: '{}'", name)));
                        }
                    }

                    // 2. Check that all parameters are provided.
                    if named.len() != sig.params.len() {
                        return Err(serr(
                            *span,
                            format!(
                                "argument count mismatch for named call: expected {}, got {}",
                                sig.params.len(),
                                named.len()
                            ),
                        ));
                    }

                    // 3. Check types.
                    for (pname, pty) in &sig.params {
                        let (expr, arg_sp) = named.get(pname).unwrap(); // Safe due to checks above.
                        let ety = check_expr(expr, fns, methods, scopes, structs)?;
                        if ety != *pty {
                            return Err(serr(*arg_sp, "arg type mismatch"));
                        }
                    }
                    Ok(sig.ret.clone())
                } else {
                    if pos.len() != sig.params.len() {
                        return Err(serr(
                            *span,
                            format!(
                                "arg count mismatch: expected {}, got {}",
                                sig.params.len(),
                                pos.len()
                            ),
                        ));
                    }
                    for (i, x) in pos.iter().enumerate() {
                        let xt = check_expr(x, fns, methods, scopes, structs)?;
                        if xt != sig.params[i].1 {
                            return Err(serr(x.span(), "arg type mismatch"));
                        }
                    }
                    Ok(sig.ret.clone())
                }
            } else {
                Err(serr(*span, "invalid call target"))
            }
        }
    }
}
