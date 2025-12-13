use std::collections::HashMap;
use crate::ast::*;
use crate::lex::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Ty { Int, Bool, Text, Void }

#[derive(Debug)]
pub struct FnSig { pub ret: Ty, pub params: Vec<(String, Ty)> }

#[derive(Debug)]
pub struct SemInfo { pub fns: HashMap<String, FnSig> }

#[derive(Debug)]
pub struct SemError { pub span: Span, pub msg: String }
impl std::fmt::Display for SemError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.line, self.span.col, self.msg)
    }
}
impl std::error::Error for SemError {}
fn serr(span: Span, msg: impl Into<String>) -> SemError { SemError { span, msg: msg.into() } }

pub fn check(prog: &Program) -> Result<SemInfo, SemError> {
    let mut fns: HashMap<String, FnSig> = HashMap::new();

    for f in &prog.funcs {
        if fns.contains_key(&f.name) {
            return Err(serr(f.span, format!("duplicate function: {}", f.name)));
        }
        let ret = match &f.ret_ty {
            None => Ty::Void,
            Some(s) => parse_ty(s).ok_or_else(|| serr(f.span, format!("unknown type: {s}")))?,
        };
        let mut params = Vec::new();
        let mut seen = HashMap::<String, Span>::new();
        for p in &f.params {
            if let Some(prev) = seen.insert(p.name.clone(), p.span) {
                return Err(serr(p.span, format!("duplicate param: {} (previous at {}:{})", p.name, prev.line, prev.col)));
            }
            let t = parse_ty(&p.ty).ok_or_else(|| serr(p.span, format!("unknown type: {}", p.ty)))?;
            params.push((p.name.clone(), t));
        }
        fns.insert(f.name.clone(), FnSig { ret, params });
    }

    // main must exist and be int
    let main_sig = fns.get("main").ok_or_else(|| serr(Span{line:1,col:1}, "missing function: main"))?;
    if main_sig.ret != Ty::Int {
        return Err(serr(Span{line:1,col:1}, "main must return intg/int (i64)"));
    }

    // per-function check
    for f in &prog.funcs {
        let sig = fns.get(&f.name).unwrap();
        check_func(f, sig, &fns)?;
    }

    Ok(SemInfo { fns })
}

fn parse_ty(s: &str) -> Option<Ty> {
    Some(match s {
        "intg" | "int" => Ty::Int,
        "bool" | "bol" => Ty::Bool,
        "text" | "txt" => Ty::Text,
        "void" | "vod" => Ty::Void,
        _ => return None,
    })
}

#[derive(Clone)]
struct VarInfo { ty: Ty, mutable: bool }

fn check_func(f: &Func, sig: &FnSig, fns: &HashMap<String, FnSig>) -> Result<(), SemError> {
    let mut scopes: Vec<HashMap<String, VarInfo>> = vec![HashMap::new()];

    // params as immutable vars
    for (name, ty) in &sig.params {
        scopes[0].insert(name.clone(), VarInfo { ty: *ty, mutable: false });
    }

    let mut loop_depth = 0usize;
    let mut saw_return = false;

    for st in &f.body {
        saw_return |= check_stmt(st, sig.ret, fns, &mut scopes, &mut loop_depth)?;
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
    ret_ty: Ty,
    fns: &HashMap<String, FnSig>,
    scopes: &mut Vec<HashMap<String, VarInfo>>,
    loop_depth: &mut usize,
) -> Result<bool, SemError> {
    match st {
        Stmt::VarDecl { mutable, name, ty, init, span } => {
            let decl_ty = parse_ty(ty).ok_or_else(|| serr(*span, format!("unknown type: {ty}")))?;
            let init_ty = check_expr(init, fns, scopes)?;
            if decl_ty != init_ty {
                return Err(serr(*span, format!("type mismatch: {} := {:?} (init is {:?})", name, decl_ty, init_ty)));
            }
            scopes.last_mut().unwrap().insert(name.clone(), VarInfo { ty: decl_ty, mutable: *mutable });
            Ok(false)
        }
        Stmt::Assign { name, expr, span } => {
            let (vty, mutability) = lookup_var(name, *span, scopes)?;
            if !mutability { return Err(serr(*span, format!("cannot assign to lock: {name}"))); }
            let ety = check_expr(expr, fns, scopes)?;
            if vty != ety { return Err(serr(*span, format!("type mismatch in assignment to {name}"))); }
            Ok(false)
        }
        Stmt::Expr { expr, span } => {
            // MVP: statement expression must be a call
            match expr {
                Expr::Call { .. } => { let _ = check_expr(expr, fns, scopes)?; Ok(false) }
                _ => Err(serr(*span, "only function calls are allowed as expression statements")),
            }
        }
        Stmt::Back { expr, span } => {
            match ret_ty {
                Ty::Void => {
                    if expr.is_some() { return Err(serr(*span, "void function cannot return a value")); }
                }
                _ => {
                    let e = expr.as_ref().ok_or_else(|| serr(*span, "non-void function must return a value"))?;
                    let ety = check_expr(e, fns, scopes)?;
                    if ety != ret_ty { return Err(serr(*span, "return type mismatch")); }
                }
            }
            Ok(true)
        }
        Stmt::If { arms, else_body, .. } => {
            for (c, b) in arms {
                let cty = check_expr(c, fns, scopes)?;
                if cty != Ty::Bool { return Err(serr(c.span(), "when/elif condition must be bool")); }
                scopes.push(HashMap::new());
                let mut local_ret = false;
                for s in b { local_ret |= check_stmt(s, ret_ty, fns, scopes, loop_depth)?; }
                scopes.pop();
                let _ = local_ret;
            }
            if let Some(b) = else_body {
                scopes.push(HashMap::new());
                let mut local_ret = false;
                for s in b { local_ret |= check_stmt(s, ret_ty, fns, scopes, loop_depth)?; }
                scopes.pop();
                let _ = local_ret;
            }
            Ok(false)
        }
        Stmt::While { cond, body, .. } => {
            let cty = check_expr(cond, fns, scopes)?;
            if cty != Ty::Bool { return Err(serr(cond.span(), "loop condition must be bool")); }
            *loop_depth += 1;
            scopes.push(HashMap::new());
            let mut local_ret = false;
            for s in body { local_ret |= check_stmt(s, ret_ty, fns, scopes, loop_depth)?; }
            scopes.pop();
            *loop_depth -= 1;
            let _ = local_ret;
            Ok(false)
        }
        Stmt::Break { span } => {
            if *loop_depth == 0 { return Err(serr(*span, "stop/brk is only allowed inside loop")); }
            Ok(true)
        }
        Stmt::Continue { span } => {
            if *loop_depth == 0 { return Err(serr(*span, "next/cnt is only allowed inside loop")); }
            Ok(true)
        }
    }
}

fn lookup_var(name: &str, sp: Span, scopes: &Vec<HashMap<String, VarInfo>>) -> Result<(Ty, bool), SemError> {
    for scope in scopes.iter().rev() {
        if let Some(v) = scope.get(name) { return Ok((v.ty, v.mutable)); }
    }
    Err(serr(sp, format!("unknown variable: {name}")))
}

fn check_expr(e: &Expr, fns: &HashMap<String, FnSig>, scopes: &Vec<HashMap<String, VarInfo>>) -> Result<Ty, SemError> {
    match e {
        Expr::Int(_, _) => Ok(Ty::Int),
        Expr::Str(_, _) => Ok(Ty::Text),
        Expr::Ident(name, sp) => {
            for scope in scopes.iter().rev() {
                if let Some(v) = scope.get(name) { return Ok(v.ty); }
            }
            Err(serr(*sp, format!("unknown identifier: {name}")))
        }
        Expr::BuiltinEcho(_) => Ok(Ty::Void),
        Expr::Unary { op, expr, span } => {
            let t = check_expr(expr, fns, scopes)?;
            match op {
                UnOp::Neg if t == Ty::Int => Ok(Ty::Int),
                UnOp::Not if t == Ty::Bool => Ok(Ty::Bool),
                _ => Err(serr(*span, "invalid unary op/type")),
            }
        }
        Expr::Binary { op, lhs, rhs, span } => {
            let a = check_expr(lhs, fns, scopes)?;
            let b = check_expr(rhs, fns, scopes)?;
            use BinOp::*;
            let r = match op {
                Add|Sub|Mul|Div|Mod|Shl|Shr|BitAnd|BitXor|BitOr => { if a==Ty::Int && b==Ty::Int { Ty::Int } else { return Err(serr(*span,"int op requires int")); } }
                Lt|Le|Gt|Ge|Eq|Ne => { if a==Ty::Int && b==Ty::Int { Ty::Bool } else { return Err(serr(*span,"cmp requires int")); } }
                And|Or => { if a==Ty::Bool && b==Ty::Bool { Ty::Bool } else { return Err(serr(*span,"logic requires bool")); } }
            };
            Ok(r)
        }
        Expr::Call { callee, args, span } => {
            // builtin echo(...)
            if matches!(&**callee, Expr::BuiltinEcho(_)) {
                if args.len() != 1 { return Err(serr(*span, "echo takes exactly 1 arg")); }
                match &args[0] {
                    Arg::Pos(x) => {
                        let t = check_expr(x, fns, scopes)?;
                        if t != Ty::Int && t != Ty::Text { return Err(serr(x.span(), "echo arg must be int or text")); }
                        Ok(Ty::Void)
                    }
                    Arg::Named{..} => Err(serr(*span, "echo does not take named args")),
                }
            } else if let Expr::Ident(fname, sp) = &**callee {
                let sig = fns.get(fname).ok_or_else(|| serr(*sp, format!("unknown function: {fname}")))?;
                // args: positional only for now (named is parsed but we validate “no mixing” already)
                let mut pos = Vec::new();
                let mut named = HashMap::<String, Span>::new();
                for a in args {
                    match a {
                        Arg::Pos(x) => pos.push(x),
                        Arg::Named{name, span, ..} => {
                            if named.insert(name.clone(), *span).is_some() {
                                return Err(serr(*span, "duplicate named arg"));
                            }
                        }
                    }
                }
                if !named.is_empty() && !pos.is_empty() {
                    return Err(serr(*span, "cannot mix named and positional args"));
                }
                if !named.is_empty() {
                    // いまは named args は sem で拒否（実装は次で入れられる）
                    return Err(serr(*span, "named args are reserved (not implemented yet)"));
                }
                if pos.len() != sig.params.len() {
                    return Err(serr(*span, format!("arg count mismatch: expected {}, got {}", sig.params.len(), pos.len())));
                }
                for (i, x) in pos.iter().enumerate() {
                    let xt = check_expr(x, fns, scopes)?;
                    if xt != sig.params[i].1 { return Err(serr(x.span(), "arg type mismatch")); }
                }
                Ok(sig.ret)
            } else {
                Err(serr(*span, "invalid call target"))
            }
        }
    }
}
