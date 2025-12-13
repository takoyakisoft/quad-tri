use std::collections::HashMap;
use crate::ast::*;
use crate::lex::Span;
use crate::sem::{FnSig, SemInfo, Ty};

#[derive(Debug)]
pub struct EmitError { pub span: Span, pub msg: String }
impl std::fmt::Display for EmitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.line, self.span.col, self.msg)
    }
}
impl std::error::Error for EmitError {}
fn eerr(span: Span, msg: impl Into<String>) -> EmitError { EmitError { span, msg: msg.into() } }

pub fn emit_module(prog: &Program, sem: &SemInfo) -> Result<String, EmitError> {
    // collect & dedup string literals
    let mut str_map: HashMap<String, (String, String, usize)> = HashMap::new(); // text -> (gname, esc, nbytes)
    let mut next_str = 0usize;
    collect_strings(prog, &mut str_map, &mut next_str);

    let mut out = String::new();
    out.push_str("; quad0 stage0\n\n");
    out.push_str("declare void @quad_echo_i64(i64)\n");
    out.push_str("declare void @quad_echo_cstr(ptr)\n\n");

    // globals
    let mut globals: Vec<(String,String,usize)> = str_map.values().cloned().collect();
    globals.sort_by(|a,b| a.0.cmp(&b.0));
    for (gname, esc, n) in &globals {
        out.push_str(&format!("{gname} = private unnamed_addr constant [{n} x i8] c\"{esc}\"\n"));
    }
    if !globals.is_empty() { out.push('\n'); }

    // function defs
    for f in &prog.funcs {
        let sig = sem.fns.get(&f.name).ok_or_else(|| eerr(f.span, "missing sem sig"))?;
        emit_func(&mut out, f, sig, &str_map, sem)?;
        out.push('\n');
    }

    Ok(out)
}

fn ty_llvm(t: Ty) -> &'static str {
    match t {
        Ty::Int => "i64",
        Ty::Bool => "i1",
        Ty::Text => "ptr",
        Ty::Void => "void",
    }
}

fn mangle(name: &str) -> String {
    format!("quad_{name}")
}

#[derive(Clone)]
struct VarInfo { ty: Ty, ptr: String, mutable: bool }

struct FnCtx<'a> {
    out: String,
    tmp: usize,
    lbl: usize,
    vars: Vec<HashMap<String, VarInfo>>,
    loops: Vec<(String,String)>, // (break_lbl, continue_lbl)
    str_map: &'a HashMap<String, (String, String, usize)>,
    ret: Ty,
    fns: &'a HashMap<String, FnSig>,
}

fn emit_func(
    module: &mut String,
    f: &Func,
    sig: &crate::sem::FnSig,
    str_map: &HashMap<String, (String, String, usize)>,
    sem: &SemInfo,
) -> Result<(), EmitError> {
    let fname = mangle(&f.name);
    let params_ir = sig.params.iter()
        .map(|(n,t)| format!("{} %{}", ty_llvm(*t), n))
        .collect::<Vec<_>>().join(", ");

    module.push_str(&format!("define {} @{fname}({params_ir}) {{\n", ty_llvm(sig.ret)));
    module.push_str("entry:\n");

    let mut ctx = FnCtx {
        out: String::new(),
        tmp: 0,
        lbl: 0,
        vars: vec![HashMap::new()],
        loops: vec![],
        str_map,
        ret: sig.ret,
        fns: &sem.fns,
    };
    // ↑ semはここでは未使用（簡略のため）。必要なら後で整理。

    // params -> alloca (uniform)
    for (name, ty) in &sig.params {
        let ptr = ctx.new_tmp("paddr");
        ctx.out.push_str(&format!("  {ptr} = alloca {}\n", ty_llvm(*ty)));
        ctx.out.push_str(&format!("  store {} %{}, ptr {ptr}\n", ty_llvm(*ty), name));
        ctx.vars[0].insert(name.clone(), VarInfo { ty: *ty, ptr, mutable: false });
    }

    let terminated = ctx.emit_block(&f.body)?;
    if !terminated {
        // default return
        match sig.ret {
            Ty::Void => ctx.out.push_str("  ret void\n"),
            Ty::Int => ctx.out.push_str("  ret i64 0\n"),
            Ty::Bool => ctx.out.push_str("  ret i1 0\n"),
            Ty::Text => ctx.out.push_str("  ret ptr null\n"),
        }
    }

    module.push_str(&ctx.out);
    module.push_str("}\n");
    Ok(())
}

fn collect_strings(prog: &Program, map: &mut HashMap<String,(String,String,usize)>, next: &mut usize) {
    fn walk_expr(e: &Expr, map: &mut HashMap<String,(String,String,usize)>, next: &mut usize) {
        match e {
            Expr::Str(s, _) => {
                if !map.contains_key(s) {
                    let g = format!("@.str{}", *next);
                    *next += 1;
                    let esc = escape_c_string(s);
                    let n = unescape_to_bytes(&esc).len();
                    map.insert(s.clone(), (g, esc, n));
                }
            }
            Expr::Call { callee, args, .. } => {
                walk_expr(callee, map, next);
                for a in args {
                    match a {
                        Arg::Pos(x) => walk_expr(x, map, next),
                        Arg::Named{expr, ..} => walk_expr(expr, map, next),
                    }
                }
            }
            Expr::Unary { expr, .. } => walk_expr(expr, map, next),
            Expr::Binary { lhs, rhs, .. } => { walk_expr(lhs, map, next); walk_expr(rhs, map, next); }
            _ => {}
        }
    }
    fn walk_stmt(s: &Stmt, map: &mut HashMap<String,(String,String,usize)>, next: &mut usize) {
        match s {
            Stmt::VarDecl{init, ..} => walk_expr(init, map, next),
            Stmt::Assign{expr, ..} => walk_expr(expr, map, next),
            Stmt::Expr{expr, ..} => walk_expr(expr, map, next),
            Stmt::Back{expr, ..} => if let Some(e) = expr { walk_expr(e, map, next) },
            Stmt::If{arms, else_body, ..} => {
                for (c,b) in arms { walk_expr(c, map, next); for x in b { walk_stmt(x, map, next); } }
                if let Some(b) = else_body { for x in b { walk_stmt(x, map, next); } }
            }
            Stmt::While{cond, body, ..} => { walk_expr(cond, map, next); for x in body { walk_stmt(x, map, next); } }
            _ => {}
        }
    }
    for f in &prog.funcs { for s in &f.body { walk_stmt(s, map, next); } }
}

impl<'a> FnCtx<'a> {
    fn new_tmp(&mut self, p: &str) -> String {
        let s = format!("%{p}{}", self.tmp);
        self.tmp += 1;
        s
    }
    fn new_lbl(&mut self, p: &str) -> String {
        let s = format!("{p}{}", self.lbl);
        self.lbl += 1;
        s
    }
    fn push_scope(&mut self) { self.vars.push(HashMap::new()); }
    fn pop_scope(&mut self) { self.vars.pop(); }

    fn lookup(&self, name: &str) -> Option<VarInfo> {
        for sc in self.vars.iter().rev() {
            if let Some(v) = sc.get(name) { return Some(v.clone()); }
        }
        None
    }

    fn emit_block(&mut self, body: &[Stmt]) -> Result<bool, EmitError> {
        for st in body {
            let term = self.emit_stmt(st)?;
            if term { return Ok(true); }
        }
        Ok(false)
    }

    fn emit_stmt(&mut self, st: &Stmt) -> Result<bool, EmitError> {
        match st {
            Stmt::VarDecl { mutable, name, ty, init, span } => {
                let vty = match ty.as_str() {
                    "intg"|"int" => Ty::Int,
                    "bool"|"bol" => Ty::Bool,
                    "text"|"txt" => Ty::Text,
                    "void"|"vod" => Ty::Void,
                    _ => return Err(eerr(*span, format!("unknown type: {ty}"))),
                };
                let ptr = self.new_tmp("v");
                self.out.push_str(&format!("  {ptr} = alloca {}\n", ty_llvm(vty)));
                let (ity, ival) = self.emit_expr(init)?;
                if ity != vty { return Err(eerr(*span, "type mismatch in vardecl")); }
                self.out.push_str(&format!("  store {} {ival}, ptr {ptr}\n", ty_llvm(vty)));
                self.vars.last_mut().unwrap().insert(name.clone(), VarInfo { ty: vty, ptr, mutable: *mutable });
                Ok(false)
            }
            Stmt::Assign { name, expr, span } => {
                let v = self.lookup(name).ok_or_else(|| eerr(*span, format!("unknown var: {name}")))?;
                if !v.mutable { return Err(eerr(*span, format!("cannot assign to lock: {name}"))); }
                let (ty, val) = self.emit_expr(expr)?;
                if ty != v.ty { return Err(eerr(*span, "type mismatch in assignment")); }
                self.out.push_str(&format!("  store {} {val}, ptr {}\n", ty_llvm(v.ty), v.ptr));
                Ok(false)
            }
            Stmt::Expr { expr, .. } => {
                let _ = self.emit_expr(expr)?;
                Ok(false)
            }
            Stmt::Back { expr, span } => {
                match self.ret {
                    Ty::Void => {
                        if expr.is_some() { return Err(eerr(*span, "void return with value")); }
                        self.out.push_str("  ret void\n");
                    }
                    Ty::Int => {
                        let e = expr.as_ref().ok_or_else(|| eerr(*span, "missing return value"))?;
                        let (ty, val) = self.emit_expr(e)?;
                        if ty != Ty::Int { return Err(eerr(*span, "return type mismatch")); }
                        self.out.push_str(&format!("  ret i64 {val}\n"));
                    }
                    Ty::Bool => {
                        let e = expr.as_ref().ok_or_else(|| eerr(*span, "missing return value"))?;
                        let (ty, val) = self.emit_expr(e)?;
                        if ty != Ty::Bool { return Err(eerr(*span, "return type mismatch")); }
                        self.out.push_str(&format!("  ret i1 {val}\n"));
                    }
                    Ty::Text => {
                        let e = expr.as_ref().ok_or_else(|| eerr(*span, "missing return value"))?;
                        let (ty, val) = self.emit_expr(e)?;
                        if ty != Ty::Text { return Err(eerr(*span, "return type mismatch")); }
                        self.out.push_str(&format!("  ret ptr {val}\n"));
                    }
                }
                Ok(true)
            }
            Stmt::If { arms, else_body, .. } => {
                let end = self.new_lbl("if.end");
                let mut next = self.new_lbl("if.next");

                // jump into first test block
                self.out.push_str(&format!("  br label %{next}\n"));
                self.out.push_str(&format!("{next}:\n"));

                for (i, (cond, body)) in arms.iter().enumerate() {
                    let then_lbl = self.new_lbl("if.then");
                    let else_lbl = if i + 1 < arms.len() || else_body.is_some() {
                        self.new_lbl("if.else")
                    } else {
                        end.clone()
                    };

                    let (cty, cval) = self.emit_expr(cond)?;
                    if cty != Ty::Bool { return Err(eerr(cond.span(), "if cond must be bool")); }
                    self.out.push_str(&format!("  br i1 {cval}, label %{then_lbl}, label %{else_lbl}\n"));

                    self.out.push_str(&format!("{then_lbl}:\n"));
                    self.push_scope();
                    let term = self.emit_block(body)?;
                    self.pop_scope();
                    if !term { self.out.push_str(&format!("  br label %{end}\n")); }

                    if else_lbl == end { break; }
                    self.out.push_str(&format!("{else_lbl}:\n"));
                    next = else_lbl;
                }

                if let Some(b) = else_body {
                    self.push_scope();
                    let term = self.emit_block(b)?;
                    self.pop_scope();
                    if !term { self.out.push_str(&format!("  br label %{end}\n")); }
                } else {
                    // fallthrough to end if last else block exists
                    // (already branched)
                }

                self.out.push_str(&format!("{end}:\n"));
                Ok(false)
            }
            Stmt::While { cond, body, .. } => {
                let cond_lbl = self.new_lbl("while.cond");
                let body_lbl = self.new_lbl("while.body");
                let end_lbl = self.new_lbl("while.end");

                self.out.push_str(&format!("  br label %{cond_lbl}\n"));
                self.out.push_str(&format!("{cond_lbl}:\n"));
                let (cty, cval) = self.emit_expr(cond)?;
                if cty != Ty::Bool { return Err(eerr(cond.span(), "loop cond must be bool")); }
                self.out.push_str(&format!("  br i1 {cval}, label %{body_lbl}, label %{end_lbl}\n"));

                self.out.push_str(&format!("{body_lbl}:\n"));
                self.loops.push((end_lbl.clone(), cond_lbl.clone()));
                self.push_scope();
                let term = self.emit_block(body)?;
                self.pop_scope();
                self.loops.pop();
                if !term { self.out.push_str(&format!("  br label %{cond_lbl}\n")); }

                self.out.push_str(&format!("{end_lbl}:\n"));
                Ok(false)
            }
            Stmt::Break { span } => {
                let (brk, _) = self.loops.last().ok_or_else(|| eerr(*span, "stop outside loop"))?.clone();
                self.out.push_str(&format!("  br label %{brk}\n"));
                Ok(true)
            }
            Stmt::Continue { span } => {
                let (_, cont) = self.loops.last().ok_or_else(|| eerr(*span, "next outside loop"))?.clone();
                self.out.push_str(&format!("  br label %{cont}\n"));
                Ok(true)
            }
        }
    }

    fn emit_expr(&mut self, e: &Expr) -> Result<(Ty, String), EmitError> {
        match e {
            Expr::Int(v, _) => Ok((Ty::Int, v.to_string())),
            Expr::Str(s, sp) => {
                let (gname, _esc, n) = self.str_map.get(s).ok_or_else(|| eerr(*sp, "missing string global"))?.clone();
                let p = self.new_tmp("sp");
                self.out.push_str(&format!("  {p} = getelementptr inbounds [{n} x i8], ptr {gname}, i64 0, i64 0\n"));
                Ok((Ty::Text, p))
            }
            Expr::Ident(name, sp) => {
                let v = self.lookup(name).ok_or_else(|| eerr(*sp, format!("unknown var: {name}")))?;
                let t = self.new_tmp("ld");
                self.out.push_str(&format!("  {t} = load {}, ptr {}\n", ty_llvm(v.ty), v.ptr));
                Ok((v.ty, t))
            }
            Expr::BuiltinEcho(_) => Ok((Ty::Void, "0".into())),
            Expr::Unary { op, expr, span } => {
                let (t, v) = self.emit_expr(expr)?;
                match op {
                    UnOp::Neg => {
                        if t != Ty::Int { return Err(eerr(*span, "neg requires int")); }
                        let r = self.new_tmp("neg");
                        self.out.push_str(&format!("  {r} = sub i64 0, {v}\n"));
                        Ok((Ty::Int, r))
                    }
                    UnOp::Not => {
                        if t != Ty::Bool { return Err(eerr(*span, "not requires bool")); }
                        let r = self.new_tmp("not");
                        self.out.push_str(&format!("  {r} = xor i1 {v}, true\n"));
                        Ok((Ty::Bool, r))
                    }
                }
            }
            Expr::Binary { op, lhs, rhs, span } => {
                let (a, av) = self.emit_expr(lhs)?;
                let (b, bv) = self.emit_expr(rhs)?;
                use BinOp::*;
                let r = self.new_tmp("t");
                match op {
                    Add|Sub|Mul|Div|Mod|Shl|Shr|BitAnd|BitXor|BitOr => {
                        if a != Ty::Int || b != Ty::Int { return Err(eerr(*span, "int op requires int")); }
                        let ins = match op {
                            Add => "add", Sub => "sub", Mul => "mul",
                            Div => "sdiv", Mod => "srem",
                            Shl => "shl", Shr => "ashr",
                            BitAnd => "and", BitXor => "xor", BitOr => "or",
                            _ => unreachable!(),
                        };
                        self.out.push_str(&format!("  {r} = {ins} i64 {av}, {bv}\n"));
                        Ok((Ty::Int, r))
                    }
                    Lt|Le|Gt|Ge|Eq|Ne => {
                        if a != Ty::Int || b != Ty::Int { return Err(eerr(*span, "cmp requires int")); }
                        let pred = match op {
                            Lt => "slt", Le => "sle", Gt => "sgt", Ge => "sge", Eq => "eq", Ne => "ne",
                            _ => unreachable!(),
                        };
                        self.out.push_str(&format!("  {r} = icmp {pred} i64 {av}, {bv}\n"));
                        Ok((Ty::Bool, r))
                    }
                    And|Or => {
                        if a != Ty::Bool || b != Ty::Bool { return Err(eerr(*span, "logic requires bool")); }
                        let ins = if *op == And { "and" } else { "or" };
                        self.out.push_str(&format!("  {r} = {ins} i1 {av}, {bv}\n"));
                        Ok((Ty::Bool, r))
                    }
                }
            }
            Expr::Call { callee, args, span } => {
                // builtin echo(...)
                if matches!(&**callee, Expr::BuiltinEcho(_)) {
                    if args.len() != 1 { return Err(eerr(*span, "echo takes exactly 1 arg")); }
                    match &args[0] {
                        Arg::Pos(x) => {
                            let (t, v) = self.emit_expr(x)?;
                            match t {
                                Ty::Int => self.out.push_str(&format!("  call void @quad_echo_i64(i64 {v})\n")),
                                Ty::Text => self.out.push_str(&format!("  call void @quad_echo_cstr(ptr {v})\n")),
                                _ => return Err(eerr(x.span(), "echo arg must be int or text")),
                            }
                            Ok((Ty::Void, "0".into()))
                        }
                        _ => Err(eerr(*span, "echo named args not supported")),
                    }
                } else if let Expr::Ident(fname, sp) = &**callee {
                    let sig = self.fns.get(fname).ok_or_else(|| eerr(*sp, format!("unknown function: {fname}")))?;

                    let mut pos = Vec::new();
                    let mut named = HashMap::<String, (&Expr, Span)>::new();
                    for a in args {
                        match a {
                            Arg::Pos(x) => pos.push(x),
                            Arg::Named { name, expr, span } => {
                                if named.insert(name.clone(), (expr, *span)).is_some() {
                                    return Err(eerr(*span, "duplicate named arg"));
                                }
                            }
                        }
                    }
                    if !pos.is_empty() && !named.is_empty() {
                        return Err(eerr(*span, "cannot mix named and positional args"));
                    }

                    let mut arg_ir = Vec::new();
                    if !named.is_empty() {
                        if named.len() != sig.params.len() {
                            return Err(eerr(*span, "named call must provide all parameters"));
                        }
                        for (pname, pty) in &sig.params {
                            let (expr, arg_sp) = named.get(pname)
                                .ok_or_else(|| eerr(*span, format!("unknown named arg: {pname}")))?;
                            let (t, v) = self.emit_expr(expr)?;
                            if t != *pty { return Err(eerr(*arg_sp, "arg type mismatch")); }
                            arg_ir.push(format!("{} {v}", ty_llvm(t)));
                        }
                    } else {
                        if pos.len() != sig.params.len() {
                            return Err(eerr(*span, format!("arg count mismatch: expected {}, got {}", sig.params.len(), pos.len())));
                        }
                        for (i, x) in pos.iter().enumerate() {
                            let (t, v) = self.emit_expr(x)?;
                            if t != sig.params[i].1 { return Err(eerr(x.span(), "arg type mismatch")); }
                            arg_ir.push(format!("{} {v}", ty_llvm(t)));
                        }
                    }

                    let callee_ir = mangle(fname);
                    if sig.ret == Ty::Void {
                        self.out.push_str(&format!("  call void @{callee_ir}({})\n", arg_ir.join(", ")));
                        Ok((Ty::Void, "0".into()))
                    } else {
                        let dst = self.new_tmp("call");
                        self.out.push_str(&format!("  {dst} = call {} @{callee_ir}({})\n", ty_llvm(sig.ret), arg_ir.join(", ")));
                        Ok((sig.ret, dst))
                    }
                } else {
                    Err(eerr(*span, "invalid call target"))
                }
            }
        }
    }
}

// LLVM c"..." 形式（末尾 \00）
fn escape_c_string(text: &str) -> String {
    let mut out = String::new();
    for &b in text.as_bytes() {
        match b {
            b'\\' => out.push_str("\\5C"),
            b'"'  => out.push_str("\\22"),
            0x20..=0x7E => out.push(b as char),
            _ => out.push_str(&format!("\\{:02X}", b)),
        }
    }
    out.push_str("\\00");
    out
}
fn unescape_to_bytes(esc: &str) -> Vec<u8> {
    let mut out = Vec::new();
    let bs = esc.as_bytes();
    let mut i = 0usize;
    while i < bs.len() {
        if bs[i] == b'\\' && i + 2 < bs.len() {
            let h1 = bs[i + 1] as char;
            let h2 = bs[i + 2] as char;
            if let (Some(a), Some(b)) = (h1.to_digit(16), h2.to_digit(16)) {
                out.push(((a << 4) | b) as u8);
                i += 3;
                continue;
            }
        }
        out.push(bs[i]);
        i += 1;
    }
    out
}
