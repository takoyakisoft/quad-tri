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
    Enum(String),
    Array { elem: Box<Ty> },

    // Heap / pointer-like value (Quad: Addr<T>, Tri: Ptr<T>)
    Ref(Box<Ty>),
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
    pub fields: Vec<(String, Ty, Visibility)>,
    pub module_id: usize,
}

#[derive(Debug, Clone)]
pub struct EnumInfo {
    pub variants: HashMap<String, Vec<Ty>>,
}

#[derive(Debug)]
pub struct SemInfo {
    pub fns: HashMap<String, FnSig>,
    pub structs: HashMap<String, StructInfo>,
    pub enums: HashMap<String, EnumInfo>,
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

fn split_top_level_commas(s: &str) -> Vec<&str> {
    let mut out = Vec::new();
    let mut depth = 0usize;
    let mut start = 0usize;
    for (i, ch) in s.char_indices() {
        match ch {
            '<' => depth += 1,
            '>' => depth = depth.saturating_sub(1),
            ',' if depth == 0 => {
                out.push(&s[start..i]);
                start = i + 1;
            }
            _ => {}
        }
    }
    out.push(&s[start..]);
    out
}

fn parse_intrinsic_enum_name(name: &str) -> Option<(&str, Vec<&str>)> {
    if let Some(rest) = name.strip_prefix("Option<") {
        let inner = rest.strip_suffix('>')?;
        return Some(("Option", vec![inner]));
    }
    if let Some(rest) = name.strip_prefix("Result<") {
        let inner = rest.strip_suffix('>')?;
        let args = split_top_level_commas(inner);
        if args.len() != 2 {
            return None;
        }
        return Some(("Result", args));
    }
    None
}

fn ensure_intrinsic_enum(
    name: &str,
    span: Span,
    structs: &HashMap<String, StructInfo>,
    enums: &mut HashMap<String, EnumInfo>,
) -> Result<(), SemError> {
    if enums.contains_key(name) {
        return Ok(());
    }

    let (base, args) = match parse_intrinsic_enum_name(name) {
        Some(x) => x,
        None => return Ok(()),
    };

    match base {
        "Option" => {
            let t_str = args[0];
            let t_ty = parse_ty(t_str, structs, &*enums)
                .ok_or_else(|| serr(span, format!("unknown type: {t_str}")))?;
            if let Ty::Enum(inner) = &t_ty {
                ensure_intrinsic_enum(inner, span, structs, enums)?;
            }

            let mut variants = HashMap::new();
            variants.insert("None".to_string(), vec![]);
            variants.insert("Some".to_string(), vec![t_ty]);
            enums.insert(name.to_string(), EnumInfo { variants });
            Ok(())
        }
        "Result" => {
            let t_str = args[0];
            let e_str = args[1];
            let t_ty = parse_ty(t_str, structs, &*enums)
                .ok_or_else(|| serr(span, format!("unknown type: {t_str}")))?;
            let e_ty = parse_ty(e_str, structs, &*enums)
                .ok_or_else(|| serr(span, format!("unknown type: {e_str}")))?;

            if let Ty::Enum(inner) = &t_ty {
                ensure_intrinsic_enum(inner, span, structs, enums)?;
            }
            if let Ty::Enum(inner) = &e_ty {
                ensure_intrinsic_enum(inner, span, structs, enums)?;
            }

            let mut variants = HashMap::new();
            variants.insert("Ok".to_string(), vec![t_ty]);
            variants.insert("Err".to_string(), vec![e_ty]);
            enums.insert(name.to_string(), EnumInfo { variants });
            Ok(())
        }
        _ => Ok(()),
    }
}

pub fn check(prog: &LinkedProgram) -> Result<SemInfo, SemError> {
    let mut fns: HashMap<String, FnSig> = HashMap::new();
    // Prelude
    fns.insert(
        "read_file".to_string(),
        FnSig {
            ret: Ty::Text,
            params: vec![("path".to_string(), Ty::Text)],
        },
    );

    let mut enums: HashMap<String, EnumInfo> = HashMap::new();
    let mut structs: HashMap<String, StructInfo> = HashMap::new();
    let mut methods: HashMap<String, HashMap<String, MethodSig>> = HashMap::new();

    for e in &prog.enums {
        if enums.contains_key(&e.name) {
            return Err(serr(e.span, format!("duplicate enum type: {}", e.name)));
        }
        enums.insert(
            e.name.clone(),
            EnumInfo {
                variants: HashMap::new(),
            },
        );
    }

    for s in &prog.structs {
        if enums.contains_key(&s.name) {
            return Err(serr(
                s.span,
                format!("type name {} is already used by an enum", s.name),
            ));
        }
        if structs.contains_key(&s.name) {
            return Err(serr(s.span, format!("duplicate struct type: {}", s.name)));
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
            let fty = parse_ty(&f.ty, &structs, &enums)
                .ok_or_else(|| serr(f.span, format!("unknown type: {}", f.ty)))?;
            if let Ty::Enum(ref name) = fty {
                ensure_intrinsic_enum(name, f.span, &structs, &mut enums)?;
            }
            fields.push((f.name.clone(), fty, f.vis));
        }
        structs.insert(
            s.name.clone(),
            StructInfo {
                fields,
                module_id: s.span.file_id,
            },
        );
    }

    for e in &prog.enums {
        let mut variants = HashMap::new();
        let mut seen = HashSet::<String>::new();
        for v in &e.variants {
            if !seen.insert(v.name.clone()) {
                return Err(serr(v.span, format!("duplicate variant: {}", v.name)));
            }
            let mut fields = Vec::new();
            for (ty_name, ty_span) in &v.fields {
                let fty = parse_ty(ty_name, &structs, &enums)
                    .ok_or_else(|| serr(*ty_span, format!("unknown type: {ty_name}")))?;
                if let Ty::Enum(ref name) = fty {
                    ensure_intrinsic_enum(name, *ty_span, &structs, &mut enums)?;
                }
                fields.push(fty);
            }
            variants.insert(v.name.clone(), fields);
        }
        enums.insert(e.name.clone(), EnumInfo { variants });
    }

    for f in &prog.funcs {
        if fns.contains_key(&f.name) {
            return Err(serr(f.span, format!("duplicate function: {}", f.name)));
        }
        let ret = match &f.ret_ty {
            None => Ty::Void,
            Some(s) => parse_ty(s, &structs, &enums)
                .ok_or_else(|| serr(f.span, format!("unknown type: {s}")))?,
        };
        if let Ty::Enum(ref name) = ret {
            ensure_intrinsic_enum(name, f.span, &structs, &mut enums)?;
        }
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
            let t = parse_ty(&p.ty, &structs, &enums)
                .ok_or_else(|| serr(p.span, format!("unknown type: {}", p.ty)))?;
            if let Ty::Enum(ref name) = t {
                ensure_intrinsic_enum(name, p.span, &structs, &mut enums)?;
            }
            params.push((p.name.clone(), t));
        }

        if let Some(method) = &f.method {
            if method.has_self {
                match params.first() {
                    Some((_, Ty::Struct(s))) if s == &method.type_name => {}
                    Some((_, other)) => {
                        return Err(serr(
                            f.span,
                            format!(
                                "self must be of type {} (got {:?})",
                                method.type_name, other
                            ),
                        ));
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
    let main_sig = fns.get("main").ok_or_else(|| {
        serr(
            Span {
                file_id: 0,
                line: 1,
                col: 1,
            },
            "missing function: main",
        )
    })?;
    if main_sig.ret != Ty::Int {
        return Err(serr(
            Span {
                file_id: 0,
                line: 1,
                col: 1,
            },
            "main must return int (i64)",
        ));
    }

    // per-function check
    for f in &prog.funcs {
        let sig = fns.get(&f.name).unwrap();
        check_func(f, sig, &fns, &mut enums, &structs, &methods)?;
    }

    Ok(SemInfo {
        fns,
        structs,
        enums,
        methods,
    })
}

pub fn parse_ty(
    s: &str,
    structs: &HashMap<String, StructInfo>,
    enums: &HashMap<String, EnumInfo>,
) -> Option<Ty> {
    fn parse<'a>(
        src: &'a str,
        structs: &HashMap<String, StructInfo>,
        enums: &HashMap<String, EnumInfo>,
    ) -> Option<Ty> {
        if let Some(rest) = src.strip_prefix('[') {
            let idx = rest.find(']')?;
            let (_len_str, tail) = rest.split_at(idx);
            let tail = tail.strip_prefix(']')?;
            let elem = parse(tail, structs, enums)?;
            return Some(Ty::Array {
                elem: Box::new(elem),
            });
        }

        // Quad pointer type
        if let Some(rest) = src.strip_prefix("Addr<") {
            let end = rest.rfind('>')?;
            let (inner_str, tail) = rest.split_at(end);
            if tail != ">" {
                return None;
            }
            let inner = parse(inner_str, structs, enums)?;
            return Some(Ty::Ref(Box::new(inner)));
        }

        // Tri pointer type
        if let Some(rest) = src.strip_prefix("Ptr<") {
            let end = rest.rfind('>')?;
            let (inner_str, tail) = rest.split_at(end);
            if tail != ">" {
                return None;
            }
            let inner = parse(inner_str, structs, enums)?;
            return Some(Ty::Ref(Box::new(inner)));
        }

        if let Some(rest) = src.strip_prefix("Option<") {
            let end = rest.rfind('>')?;
            let (inner_str, tail) = rest.split_at(end);
            if tail != ">" {
                return None;
            }
            let _ = parse(inner_str, structs, enums)?;
            return Some(Ty::Enum(format!("Option<{inner_str}>")));
        }

        if let Some(rest) = src.strip_prefix("Result<") {
            let end = rest.rfind('>')?;
            let (inner_str, tail) = rest.split_at(end);
            if tail != ">" {
                return None;
            }
            // Validate both type args exist.
            let mut depth = 0usize;
            let mut split = None;
            for (i, ch) in inner_str.char_indices() {
                match ch {
                    '<' => depth += 1,
                    '>' => depth = depth.saturating_sub(1),
                    ',' if depth == 0 => {
                        split = Some(i);
                        break;
                    }
                    _ => {}
                }
            }
            let split = split?;
            let (t_str, e_str) = inner_str.split_at(split);
            let e_str = e_str.strip_prefix(',')?;
            let _ = parse(t_str, structs, enums)?;
            let _ = parse(e_str, structs, enums)?;
            return Some(Ty::Enum(format!("Result<{t_str},{e_str}>")));
        }

        Some(match src {
            "int" => Ty::Int,
            "bool" => Ty::Bool,
            "text" => Ty::Text,
            "void" => Ty::Void,
            other => {
                if structs.contains_key(other) {
                    Ty::Struct(other.to_string())
                } else if enums.contains_key(other) {
                    Ty::Enum(other.to_string())
                } else {
                    return None;
                }
            }
        })
    }

    parse(s, structs, enums)
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
    enums: &mut HashMap<String, EnumInfo>,
    structs: &HashMap<String, StructInfo>,
    methods: &HashMap<String, HashMap<String, MethodSig>>,
) -> Result<(), SemError> {
    let mut scopes: Vec<HashMap<String, VarInfo>> = vec![HashMap::new()];
    let module_id = f.span.file_id;

    // params as immutable bindings
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
        saw_return |= check_stmt(
            st,
            &sig.ret,
            fns,
            methods,
            enums,
            structs,
            &mut scopes,
            &mut loop_depth,
            module_id,
        )?;
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
    enums: &mut HashMap<String, EnumInfo>,
    structs: &HashMap<String, StructInfo>,
    scopes: &mut Vec<HashMap<String, VarInfo>>,
    loop_depth: &mut usize,
    module_id: usize,
) -> Result<bool, SemError> {
    match st {
        Stmt::VarDecl {
            mutable,
            name,
            ty,
            init,
            span,
        } => {
            let decl_ty = parse_ty(ty, structs, &*enums)
                .ok_or_else(|| serr(*span, format!("unknown type: {ty}")))?;
            if let Ty::Enum(ref name) = decl_ty {
                ensure_intrinsic_enum(name, *span, structs, enums)?;
            }
            let init_ty = check_expr(
                init, ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
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
                    return Err(serr(
                        *span,
                        format!("cannot assign to immutable binding: {name}"),
                    ));
                }
                let ety = check_expr(
                    expr, ret_ty, fns, methods, scopes, enums, structs, module_id,
                )?;
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
                    return Err(serr(
                        *span,
                        format!("cannot assign to immutable binding: {base}"),
                    ));
                }
                let sname = match bty {
                    Ty::Struct(ref n) => n,
                    _ => {
                        return Err(serr(
                            *span,
                            format!("{base} is not a struct for field assignment"),
                        ));
                    }
                };
                let sinfo = structs.get(sname).ok_or_else(|| {
                    serr(*span, format!("unknown struct during assignment: {sname}"))
                })?;
                let mut fty = None;
                for (fname, fty_val, vis) in &sinfo.fields {
                    if fname == field {
                        if *vis == Visibility::Private && sinfo.module_id != module_id {
                            return Err(serr(*span, format!("field {field} is private")));
                        }
                        fty = Some(fty_val.clone());
                        break;
                    }
                }
                let fty = fty.ok_or_else(|| serr(*span, format!("unknown field: {field}")))?;
                let ety = check_expr(
                    expr, ret_ty, fns, methods, scopes, enums, structs, module_id,
                )?;
                if fty != ety {
                    return Err(serr(*span, "type mismatch in field assignment"));
                }
                Ok(false)
            }
            AssignTarget::Index { base, index } => {
                let (bty, mutability) = lookup_var(base, *span, scopes)?;
                if !mutability {
                    return Err(serr(
                        *span,
                        format!("cannot assign to immutable binding: {base}"),
                    ));
                }
                let ity = check_expr(
                    index, ret_ty, fns, methods, scopes, enums, structs, module_id,
                )?;
                if ity != Ty::Int {
                    return Err(serr(index.span(), "index must be int"));
                }
                let elem_ty = match bty {
                    Ty::Array { ref elem } => elem.as_ref().clone(),
                    _ => {
                        return Err(serr(
                            *span,
                            format!("{base} is not an array for index assignment"),
                        ));
                    }
                };
                let ety = check_expr(
                    expr, ret_ty, fns, methods, scopes, enums, structs, module_id,
                )?;
                if ety != elem_ty {
                    return Err(serr(*span, "type mismatch in index assignment"));
                }
                Ok(false)
            }
        },
        Stmt::Expr { expr, span } => {
            // MVP: statement expression must be a call
            match expr {
                Expr::Call { .. } => {
                    let _ = check_expr(
                        expr, ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
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
                    let ety =
                        check_expr(e, ret_ty, fns, methods, scopes, enums, structs, module_id)?;
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
                let cty = check_expr(c, ret_ty, fns, methods, scopes, enums, structs, module_id)?;
                if cty != Ty::Bool {
                    return Err(serr(c.span(), "when/elif condition must be bool"));
                }
                scopes.push(HashMap::new());
                let mut local_ret = false;
                for s in b {
                    local_ret |= check_stmt(
                        s, ret_ty, fns, methods, enums, structs, scopes, loop_depth, module_id,
                    )?;
                }
                scopes.pop();
                let _ = local_ret;
            }
            if let Some(b) = else_body {
                scopes.push(HashMap::new());
                let mut local_ret = false;
                for s in b {
                    local_ret |= check_stmt(
                        s, ret_ty, fns, methods, enums, structs, scopes, loop_depth, module_id,
                    )?;
                }
                scopes.pop();
                let _ = local_ret;
            }
            Ok(false)
        }
        Stmt::Case {
            scrutinee,
            arms,
            span: _,
        } => {
            let sty = check_expr(
                scrutinee, ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            let enum_name = match &sty {
                Ty::Enum(n) => n,
                _ => return Err(serr(scrutinee.span(), "case value must be an enum")),
            };
            ensure_intrinsic_enum(enum_name, scrutinee.span(), structs, enums)?;
            let einfo = enums
                .get(enum_name)
                .cloned()
                .ok_or_else(|| serr(scrutinee.span(), "unknown enum"))?;

            // We need to check arms.
            // Case is not a loop, so we do not increment loop_depth.

            let mut local_ret = false;

            // Track coverage if we want exhaustiveness (MVP: skip exhaustiveness check or simple check?)
            // Just check valid patterns.

            for (pat, body) in arms {
                scopes.push(HashMap::new());
                match pat {
                    Pattern::EnumVariant {
                        enum_name: pat_enum,
                        variant,
                        bindings,
                        span: pspan,
                    } => {
                        if pat_enum != enum_name {
                            return Err(serr(
                                *pspan,
                                format!("expected enum {}, got {}", enum_name, pat_enum),
                            ));
                        }
                        let fields = einfo
                            .variants
                            .get(variant)
                            .ok_or_else(|| serr(*pspan, format!("unknown variant {}", variant)))?;

                        if bindings.len() != fields.len() {
                            return Err(serr(
                                *pspan,
                                format!("expected {} fields, got {}", fields.len(), bindings.len()),
                            ));
                        }

                        // Bind variables
                        for (i, name) in bindings.iter().enumerate() {
                            scopes.last_mut().unwrap().insert(
                                name.clone(),
                                VarInfo {
                                    ty: fields[i].clone(),
                                    mutable: false, // bindings are immutable usually? Or cell?
                                                    // Let's say immutable by default in pattern.
                                },
                            );
                        }
                    }
                }

                for s in body {
                    local_ret |= check_stmt(
                        s, ret_ty, fns, methods, enums, structs, scopes, loop_depth, module_id,
                    )?;
                }
                scopes.pop();
            }
            // *loop_depth -= 1; // Removed.

            let _ = local_ret; // Case doesn't guarantee return unless exhaustive & all arms return.
            // Simplification: ignore return for now.
            Ok(false)
        }
        Stmt::While { cond, body, .. } => {
            let cty = check_expr(
                cond, ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            if cty != Ty::Bool {
                return Err(serr(cond.span(), "loop condition must be bool"));
            }
            *loop_depth += 1;
            scopes.push(HashMap::new());
            let mut local_ret = false;
            for s in body {
                local_ret |= check_stmt(
                    s, ret_ty, fns, methods, enums, structs, scopes, loop_depth, module_id,
                )?;
            }
            scopes.pop();
            *loop_depth -= 1;
            let _ = local_ret;
            Ok(false)
        }
        Stmt::Foreach {
            name,
            ty,
            iter,
            body,
            span,
        } => {
            let loop_var_ty = parse_ty(ty, structs, &*enums)
                .ok_or_else(|| serr(*span, format!("unknown type: {ty}")))?;
            if let Ty::Enum(ref enum_name) = loop_var_ty {
                ensure_intrinsic_enum(enum_name, *span, structs, enums)?;
            }

            let iter_ty = check_expr(
                iter, ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;

            let iter_struct = match iter_ty {
                Ty::Struct(n) => n,
                Ty::Ref(inner) => match *inner {
                    Ty::Struct(n) => n,
                    _ => {
                        return Err(serr(
                            iter.span(),
                            "foreach iterator must be a struct (or Addr<struct>)",
                        ));
                    }
                },
                _ => {
                    return Err(serr(
                        iter.span(),
                        "foreach iterator must be a struct (or Addr<struct>)",
                    ));
                }
            };

            let next_sig = methods
                .get(&iter_struct)
                .and_then(|m| m.get("next"))
                .ok_or_else(|| serr(iter.span(), "foreach iterator requires next() method"))?;
            if !next_sig.has_self {
                return Err(serr(iter.span(), "next() must be a method (requires self)"));
            }

            let next_ret = match &next_sig.ret {
                Ty::Enum(n) => n.as_str(),
                _ => {
                    return Err(serr(
                        iter.span(),
                        "next() must return Option<T> for foreach",
                    ));
                }
            };
            ensure_intrinsic_enum(next_ret, iter.span(), structs, enums)?;

            let (base, args) = parse_intrinsic_enum_name(next_ret)
                .ok_or_else(|| serr(iter.span(), "next() must return Option<T>"))?;
            if base != "Option" || args.len() != 1 {
                return Err(serr(iter.span(), "next() must return Option<T>"));
            }

            let inner_ty = parse_ty(args[0], structs, &*enums)
                .ok_or_else(|| serr(iter.span(), "unknown Option<T> inner type"))?;
            if let Ty::Enum(ref enum_name) = inner_ty {
                ensure_intrinsic_enum(enum_name, iter.span(), structs, enums)?;
            }
            if inner_ty != loop_var_ty {
                return Err(serr(
                    *span,
                    format!(
                        "foreach type mismatch: next() yields {:?} but loop variable is {:?}",
                        inner_ty, loop_var_ty
                    ),
                ));
            }

            *loop_depth += 1;
            scopes.push(HashMap::new());
            scopes.last_mut().unwrap().insert(
                name.clone(),
                VarInfo {
                    ty: loop_var_ty,
                    mutable: false,
                },
            );

            let mut local_ret = false;
            for s in body {
                local_ret |= check_stmt(
                    s, ret_ty, fns, methods, enums, structs, scopes, loop_depth, module_id,
                )?;
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
                return Err(serr(*span, "next/nxt is only allowed inside loop"));
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
    fn_ret_ty: &Ty,
    fns: &HashMap<String, FnSig>,
    methods: &HashMap<String, HashMap<String, MethodSig>>,
    scopes: &Vec<HashMap<String, VarInfo>>,
    enums: &mut HashMap<String, EnumInfo>,
    structs: &HashMap<String, StructInfo>,
    module_id: usize,
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
        Expr::EnumLit {
            enum_name,
            variant,
            args,
            span,
        } => {
            ensure_intrinsic_enum(enum_name, *span, structs, enums)?;
            let fields = enums
                .get(enum_name)
                .ok_or_else(|| serr(*span, format!("unknown enum: {enum_name}")))?
                .variants
                .get(variant)
                .ok_or_else(|| serr(*span, format!("unknown variant: {variant}")))?
                .clone();

            if args.len() != fields.len() {
                return Err(serr(
                    *span,
                    format!(
                        "variant {variant} expects {} args, got {}",
                        fields.len(),
                        args.len()
                    ),
                ));
            }

            for (i, expr) in args.iter().enumerate() {
                let ety = check_expr(
                    expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                )?;
                if ety != fields[i] {
                    return Err(serr(expr.span(), "arg type mismatch"));
                }
            }
            Ok(Ty::Enum(enum_name.clone()))
        }
        Expr::Field { base, field, span } => {
            let bty = check_expr(
                base, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            let sname = match bty {
                Ty::Struct(n) => n,
                Ty::Ref(inner) => match *inner {
                    Ty::Struct(n) => n,
                    _ => return Err(serr(*span, "field access on non-struct")),
                },
                _ => return Err(serr(*span, "field access on non-struct")),
            };
            let sinfo = structs
                .get(&sname)
                .ok_or_else(|| serr(*span, format!("unknown struct: {sname}")))?;
            let (fty, vis) = sinfo
                .fields
                .iter()
                .find(|(n, _, _)| n == field)
                .map(|(_, t, v)| (t.clone(), *v))
                .ok_or_else(|| serr(*span, format!("unknown field: {field}")))?;

            if vis == Visibility::Private && sinfo.module_id != module_id {
                return Err(serr(*span, format!("field {field} is private")));
            }

            Ok(fty)
        }
        Expr::Index { base, index, span } => {
            let bty = check_expr(
                base, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            let ity = check_expr(
                index, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            if ity != Ty::Int {
                return Err(serr(index.span(), "index must be int"));
            }
            match bty {
                Ty::Array { elem } => Ok(*elem),
                Ty::Ref(inner) => match *inner {
                    Ty::Array { elem } => Ok(*elem),
                    _ => Err(serr(*span, "indexing is only supported on arrays")),
                },
                _ => Err(serr(*span, "indexing is only supported on arrays")),
            }
        }
        Expr::ArrayLit { elements, span } => {
            if elements.is_empty() {
                return Err(serr(*span, "array literal cannot be empty"));
            }
            let first = check_expr(
                &elements[0],
                fn_ret_ty,
                fns,
                methods,
                scopes,
                enums,
                structs,
                module_id,
            )?;
            for e in elements.iter().skip(1) {
                let t = check_expr(
                    e, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                )?;
                if t != first {
                    return Err(serr(e.span(), "array elements must match"));
                }
            }
            Ok(Ty::Array {
                elem: Box::new(first),
            })
        }
        Expr::Unary { op, expr, span } => {
            let t = check_expr(
                expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            match op {
                UnOp::Neg if t == Ty::Int => Ok(Ty::Int),
                UnOp::Not if t == Ty::Bool => Ok(Ty::Bool),
                _ => Err(serr(*span, "invalid unary op/type")),
            }
        }
        Expr::Binary { op, lhs, rhs, span } => {
            let a = check_expr(
                lhs, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            let b = check_expr(
                rhs, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            use BinOp::*;
            let r = match op {
                Add => {
                    if a == Ty::Int && b == Ty::Int {
                        Ty::Int
                    } else if a == Ty::Text && b == Ty::Text {
                        Ty::Text
                    } else {
                        return Err(serr(*span, "invalid + operands"));
                    }
                }
                Sub | Mul | Div | Mod | Shl | Shr | BitAnd | BitXor | BitOr => {
                    if a == Ty::Int && b == Ty::Int {
                        Ty::Int
                    } else {
                        return Err(serr(*span, "int op requires int"));
                    }
                }
                Lt | Le | Gt | Ge => {
                    if a == Ty::Int && b == Ty::Int {
                        Ty::Bool
                    } else {
                        return Err(serr(*span, "cmp requires int"));
                    }
                }
                Eq | Ne => {
                    if (a == Ty::Int && b == Ty::Int)
                        || (a == Ty::Bool && b == Ty::Bool)
                        || (a == Ty::Text && b == Ty::Text)
                    {
                        Ty::Bool
                    } else {
                        return Err(serr(*span, "==/!= requires matching scalar types"));
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
        Expr::Try { expr, span } => {
            let in_ty = check_expr(
                expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            let name = match in_ty {
                Ty::Enum(n) => n,
                _ => return Err(serr(*span, "'?' requires Option<T> or Result<T,E>")),
            };

            ensure_intrinsic_enum(&name, *span, structs, enums)?;

            let (base, args) = parse_intrinsic_enum_name(&name)
                .ok_or_else(|| serr(*span, "'?' requires Option<T> or Result<T,E>"))?;

            match base {
                "Option" => {
                    let ret_name = match fn_ret_ty {
                        Ty::Enum(n) => n.as_str(),
                        _ => {
                            return Err(serr(
                                *span,
                                "'?' in Option requires function to return Option",
                            ));
                        }
                    };
                    if parse_intrinsic_enum_name(ret_name).map(|p| p.0) != Some("Option") {
                        return Err(serr(
                            *span,
                            "'?' in Option requires function to return Option",
                        ));
                    }
                    let t = parse_ty(args[0], structs, &*enums)
                        .ok_or_else(|| serr(*span, "unknown Option<T> inner type"))?;
                    Ok(t)
                }
                "Result" => {
                    let ret_name = match fn_ret_ty {
                        Ty::Enum(n) => n.as_str(),
                        _ => {
                            return Err(serr(
                                *span,
                                "'?' in Result requires function to return Result",
                            ));
                        }
                    };
                    let (ret_base, ret_args) =
                        parse_intrinsic_enum_name(ret_name).ok_or_else(|| {
                            serr(*span, "'?' in Result requires function to return Result")
                        })?;
                    if ret_base != "Result" {
                        return Err(serr(
                            *span,
                            "'?' in Result requires function to return Result",
                        ));
                    }
                    let in_e = parse_ty(args[1], structs, &*enums)
                        .ok_or_else(|| serr(*span, "unknown Result<T,E> error type"))?;
                    let ret_e = parse_ty(ret_args[1], structs, &*enums)
                        .ok_or_else(|| serr(*span, "unknown Result<_,E> error type"))?;
                    if in_e != ret_e {
                        return Err(serr(*span, "'?' requires matching Result error type"));
                    }
                    let t = parse_ty(args[0], structs, &*enums)
                        .ok_or_else(|| serr(*span, "unknown Result<T,_> ok type"))?;
                    Ok(t)
                }
                _ => Err(serr(*span, "'?' requires Option<T> or Result<T,E>")),
            }
        }
        Expr::Call { callee, args, span } => {
            // builtin print(...) / println(...)
            if let Expr::Ident(fname, _) = &**callee {
                if (fname == "print" || fname == "println") && !fns.contains_key(fname) {
                    if args.len() != 1 {
                        return Err(serr(*span, format!("{fname} takes exactly 1 arg")));
                    }
                    match &args[0] {
                        Arg::Pos(x) => {
                            let t = check_expr(
                                x, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                            )?;
                            if t != Ty::Int && t != Ty::Text {
                                return Err(serr(
                                    x.span(),
                                    format!("{fname} arg must be int or text"),
                                ));
                            }
                            return Ok(Ty::Void);
                        }
                        Arg::Named { .. } => {
                            return Err(serr(*span, format!("{fname} does not take named args")));
                        }
                    }
                }
            }

            // Struct construction via parens: `TypeName(...)`
            // Resolved in semantics so the parser stays type-agnostic.
            if let Expr::Ident(type_name, _) = &**callee {
                if let Some(sinfo) = structs.get(type_name) {
                    if args.iter().all(|a| matches!(a, Arg::Pos(_))) {
                        if args.len() != sinfo.fields.len() {
                            return Err(serr(
                                *span,
                                format!(
                                    "struct {type_name} expects {} args, got {}",
                                    sinfo.fields.len(),
                                    args.len()
                                ),
                            ));
                        }
                        for (i, a) in args.iter().enumerate() {
                            let Arg::Pos(expr) = a else { unreachable!() };
                            let (fname, fty, vis) = &sinfo.fields[i];

                            if *vis == Visibility::Private && sinfo.module_id != module_id {
                                return Err(serr(expr.span(), format!("field {fname} is private")));
                            }

                            let ety = check_expr(
                                expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                            )?;
                            if ety != fty.clone() {
                                return Err(serr(expr.span(), "field type mismatch"));
                            }
                        }
                        return Ok(Ty::Struct(type_name.clone()));
                    }

                    if args.iter().all(|a| matches!(a, Arg::Named { .. })) {
                        let mut seen = HashSet::new();

                        for a in args {
                            let Arg::Named {
                                name,
                                expr,
                                span: nspan,
                            } = a
                            else {
                                unreachable!();
                            };
                            let (fty, vis) = sinfo
                                .fields
                                .iter()
                                .find(|(n, _, _)| n == name)
                                .map(|(_, t, v)| (t.clone(), *v))
                                .ok_or_else(|| serr(*nspan, format!("unknown field: {name}")))?;

                            if vis == Visibility::Private && sinfo.module_id != module_id {
                                return Err(serr(*nspan, format!("field {name} is private")));
                            }

                            let ety = check_expr(
                                expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                            )?;
                            if ety != fty {
                                return Err(serr(*nspan, "field type mismatch"));
                            }
                            if !seen.insert(name) {
                                return Err(serr(*nspan, format!("duplicate field: {name}")));
                            }
                        }

                        if seen.len() != sinfo.fields.len() {
                            return Err(serr(*span, "missing field in struct constructor"));
                        }
                        return Ok(Ty::Struct(type_name.clone()));
                    }

                    return Err(serr(
                        *span,
                        "struct construction args must be either all positional or all named",
                    ));
                }
            }

            if let Expr::Field {
                base,
                field,
                span: field_span,
            } = &**callee
            {
                let treat_as_type = match &**base {
                    Expr::Ident(name, _) => methods.contains_key(name),
                    _ => false,
                };

                if treat_as_type {
                    let Expr::Ident(type_name, _) = &**base else {
                        unreachable!();
                    };
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
                    validate_args(
                        args,
                        &sig.params,
                        *span,
                        fn_ret_ty,
                        fns,
                        methods,
                        enums,
                        scopes,
                        structs,
                        module_id,
                    )?;
                    Ok(sig.ret.clone())
                } else {
                    let recv_ty = check_expr(
                        base, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;

                    // Hard-coded unwrap/expect for intrinsic Option/Result.
                    // Final surface names: unwrap/expect (not reserved keywords).
                    let is_unwrap = matches!(field.as_str(), "unwrap");
                    let is_expect = matches!(field.as_str(), "expect");

                    if is_unwrap || is_expect {
                        let enum_name = match &recv_ty {
                            Ty::Enum(n) => Some(n.as_str()),
                            Ty::Ref(inner) => match &**inner {
                                Ty::Enum(n) => Some(n.as_str()),
                                _ => None,
                            },
                            _ => None,
                        };

                        if let Some(enum_name) = enum_name {
                            if let Some((base_name, type_args)) =
                                parse_intrinsic_enum_name(enum_name)
                            {
                                // args validation
                                if is_unwrap {
                                    if !args.is_empty() {
                                        return Err(serr(*span, "unwrap takes no args"));
                                    }
                                } else {
                                    if args.len() != 1 {
                                        return Err(serr(*span, "expect takes exactly 1 arg"));
                                    }
                                    match &args[0] {
                                        Arg::Pos(msg) => {
                                            let mt = check_expr(
                                                msg, fn_ret_ty, fns, methods, scopes, enums,
                                                structs, module_id,
                                            )?;
                                            if mt != Ty::Text {
                                                return Err(serr(
                                                    msg.span(),
                                                    "expect(msg) requires text",
                                                ));
                                            }
                                        }
                                        Arg::Named { .. } => {
                                            return Err(serr(
                                                *span,
                                                "expect does not take named args",
                                            ));
                                        }
                                    }
                                }

                                let ok_ty_str = match base_name {
                                    "Option" => type_args[0],
                                    "Result" => type_args[0],
                                    _ => {
                                        return Err(serr(
                                            *field_span,
                                            "unwrap/expect is only supported on Option/Result",
                                        ));
                                    }
                                };
                                let ok_ty = parse_ty(ok_ty_str, structs, &*enums)
                                    .ok_or_else(|| serr(*field_span, "unknown inner type"))?;
                                if let Ty::Enum(inner) = &ok_ty {
                                    ensure_intrinsic_enum(inner, *field_span, structs, enums)?;
                                }
                                return Ok(ok_ty);
                            }
                        }
                    }

                    let sname = match recv_ty {
                        Ty::Struct(n) => n,
                        Ty::Ref(inner) => match *inner {
                            Ty::Struct(n) => n,
                            _ => {
                                return Err(serr(
                                    *field_span,
                                    "method call requires struct receiver",
                                ));
                            }
                        },
                        _ => return Err(serr(*field_span, "method call requires struct receiver")),
                    };
                    let table = methods
                        .get(&sname)
                        .ok_or_else(|| serr(*field_span, format!("{sname} has no methods")))?;
                    let sig = table
                        .get(field)
                        .ok_or_else(|| serr(*field_span, format!("unknown method: {field}")))?;
                    if !sig.has_self {
                        return Err(serr(
                            *field_span,
                            "associated function cannot use value receiver",
                        ));
                    }
                    validate_args(
                        args,
                        &sig.params[1..],
                        *span,
                        fn_ret_ty,
                        fns,
                        methods,
                        enums,
                        scopes,
                        structs,
                        module_id,
                    )?;
                    Ok(sig.ret.clone())
                }
            } else if let Expr::Ident(fname, sp) = &**callee {
                // Builtins with type-dependent behavior
                if fname == "heap" || fname == "mem" {
                    if args.len() != 1 {
                        return Err(serr(*span, "heap takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "heap does not take named args"));
                        }
                    };
                    let inner = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if inner == Ty::Void {
                        return Err(serr(expr.span(), "cannot heap-allocate void"));
                    }
                    Ok(Ty::Ref(Box::new(inner)))
                } else if fname == "free" || fname == "del" {
                    if args.len() != 1 {
                        return Err(serr(*span, "free takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "free does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    match t {
                        Ty::Ref(_) | Ty::Text | Ty::Array { .. } => Ok(Ty::Void),
                        _ => Err(serr(expr.span(), "free expects Addr<T>, text, or array")),
                    }
                } else if fname == "deref" {
                    if args.len() != 1 {
                        return Err(serr(*span, "deref takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "deref does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    match t {
                        Ty::Ref(inner) => Ok(*inner),
                        _ => Err(serr(expr.span(), "deref expects Addr<T>")),
                    }
                } else if fname == "sys_write" || fname == "sys_writeln" {
                    if args.len() != 1 {
                        return Err(serr(*span, format!("{fname} takes exactly 1 arg")));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, format!("{fname} does not take named args")));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Text {
                        return Err(serr(expr.span(), format!("{fname} expects text")));
                    }
                    Ok(Ty::Void)
                } else if fname == "sys_read_file" {
                    if args.len() != 1 {
                        return Err(serr(*span, "sys_read_file takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "sys_read_file does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Text {
                        return Err(serr(expr.span(), "sys_read_file expects text"));
                    }
                    Ok(Ty::Text)
                } else if fname == "sys_panic" {
                    if args.len() != 1 {
                        return Err(serr(*span, "sys_panic takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "sys_panic does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Text {
                        return Err(serr(expr.span(), "sys_panic expects text"));
                    }
                    Ok(Ty::Void)
                } else if fname == "sys_exit" {
                    if args.len() != 1 {
                        return Err(serr(*span, "sys_exit takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "sys_exit does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Int {
                        return Err(serr(expr.span(), "sys_exit expects int"));
                    }
                    Ok(Ty::Void)
                } else if fname == "args_count" {
                    if !args.is_empty() {
                        return Err(serr(*span, "args_count takes no args"));
                    }
                    Ok(Ty::Int)
                } else if fname == "arg_get" {
                    if args.len() != 1 {
                        return Err(serr(*span, "arg_get takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "arg_get does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Int {
                        return Err(serr(expr.span(), "arg_get expects int"));
                    }
                    Ok(Ty::Text)
                } else if fname == "alloc_bytes" {
                    if args.len() != 1 {
                        return Err(serr(*span, "alloc_bytes takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "alloc_bytes does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Int {
                        return Err(serr(expr.span(), "alloc_bytes expects int"));
                    }
                    Ok(Ty::Int)
                } else if fname == "sys_dealloc" {
                    match args.len() {
                        1 => {
                            let expr = match &args[0] {
                                Arg::Pos(e) => e,
                                Arg::Named { .. } => {
                                    return Err(serr(
                                        *span,
                                        "sys_dealloc does not take named args",
                                    ));
                                }
                            };
                            let t = check_expr(
                                expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                            )?;
                            match t {
                                Ty::Ref(_) | Ty::Text | Ty::Array { .. } => Ok(Ty::Void),
                                _ => Err(serr(expr.span(), "sys_dealloc expects ptr-like value")),
                            }
                        }
                        2 => {
                            let p_expr = match &args[0] {
                                Arg::Pos(e) => e,
                                Arg::Named { .. } => {
                                    return Err(serr(
                                        *span,
                                        "sys_dealloc does not take named args",
                                    ));
                                }
                            };
                            let s_expr = match &args[1] {
                                Arg::Pos(e) => e,
                                Arg::Named { .. } => {
                                    return Err(serr(
                                        *span,
                                        "sys_dealloc does not take named args",
                                    ));
                                }
                            };
                            let pt = check_expr(
                                p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                            )?;
                            let st = check_expr(
                                s_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                            )?;
                            if pt != Ty::Int || st != Ty::Int {
                                return Err(serr(
                                    *span,
                                    "sys_dealloc(ptr, size) expects (int, int)",
                                ));
                            }
                            Ok(Ty::Void)
                        }
                        _ => Err(serr(
                            *span,
                            "sys_dealloc takes either 1 (managed) or 2 (ptr, size) args",
                        )),
                    }
                } else if fname == "ptr_get_int" {
                    if args.len() != 2 {
                        return Err(serr(*span, "ptr_get_int takes exactly 2 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_get_int does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_get_int does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Int || it != Ty::Int {
                        return Err(serr(*span, "ptr_get_int expects (int, int)"));
                    }
                    Ok(Ty::Int)
                } else if fname == "ptr_set_int" {
                    if args.len() != 3 {
                        return Err(serr(*span, "ptr_set_int takes exactly 3 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_set_int does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_set_int does not take named args"));
                        }
                    };
                    let v_expr = match &args[2] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_set_int does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    let vt = check_expr(
                        v_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Int || it != Ty::Int || vt != Ty::Int {
                        return Err(serr(*span, "ptr_set_int expects (int, int, int)"));
                    }
                    Ok(Ty::Void)
                } else if fname == "ptr_get_text" {
                    if args.len() != 2 {
                        return Err(serr(*span, "ptr_get_text takes exactly 2 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_get_text does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_get_text does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Int || it != Ty::Int {
                        return Err(serr(*span, "ptr_get_text expects (int, int)"));
                    }
                    Ok(Ty::Text)
                } else if fname == "ptr_set_text" {
                    if args.len() != 3 {
                        return Err(serr(*span, "ptr_set_text takes exactly 3 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_set_text does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_set_text does not take named args"));
                        }
                    };
                    let v_expr = match &args[2] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "ptr_set_text does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    let vt = check_expr(
                        v_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Int || it != Ty::Int || vt != Ty::Text {
                        return Err(serr(*span, "ptr_set_text expects (int, int, text)"));
                    }
                    Ok(Ty::Void)
                } else if fname == "alloc_int_array" {
                    if args.len() != 1 {
                        return Err(serr(*span, "alloc_int_array takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "alloc_int_array does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Int {
                        return Err(serr(expr.span(), "alloc_int_array expects int"));
                    }
                    Ok(Ty::Ref(Box::new(Ty::Int)))
                } else if fname == "alloc_text_array" {
                    if args.len() != 1 {
                        return Err(serr(*span, "alloc_text_array takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "alloc_text_array does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Int {
                        return Err(serr(expr.span(), "alloc_text_array expects int"));
                    }
                    Ok(Ty::Ref(Box::new(Ty::Text)))
                } else if fname == "itoa_native" {
                    if args.len() != 1 {
                        return Err(serr(*span, "itoa_native takes exactly 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "itoa_native does not take named args"));
                        }
                    };
                    let t = check_expr(
                        expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if t != Ty::Int {
                        return Err(serr(expr.span(), "itoa_native expects int"));
                    }
                    Ok(Ty::Text)
                } else if fname == "atoi_native" {
                    if args.len() != 2 {
                        return Err(serr(*span, "atoi_native takes exactly 2 args"));
                    }
                    let s_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "atoi_native does not take named args"));
                        }
                    };
                    let out_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "atoi_native does not take named args"));
                        }
                    };
                    let st = check_expr(
                        s_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if st != Ty::Text {
                        return Err(serr(s_expr.span(), "atoi_native expects text"));
                    }
                    let ot = check_expr(
                        out_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if ot != Ty::Ref(Box::new(Ty::Int)) {
                        return Err(serr(out_expr.span(), "atoi_native expects Addr<int>"));
                    }
                    Ok(Ty::Bool)
                } else if fname == "get_int" {
                    if args.len() != 2 {
                        return Err(serr(*span, "get_int takes exactly 2 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "get_int does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "get_int does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Ref(Box::new(Ty::Int)) {
                        return Err(serr(p_expr.span(), "get_int expects Addr<int>"));
                    }
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if it != Ty::Int {
                        return Err(serr(i_expr.span(), "get_int index must be int"));
                    }
                    Ok(Ty::Int)
                } else if fname == "set_int" {
                    if args.len() != 3 {
                        return Err(serr(*span, "set_int takes exactly 3 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "set_int does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "set_int does not take named args"));
                        }
                    };
                    let v_expr = match &args[2] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "set_int does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Ref(Box::new(Ty::Int)) {
                        return Err(serr(p_expr.span(), "set_int expects Addr<int>"));
                    }
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if it != Ty::Int {
                        return Err(serr(i_expr.span(), "set_int index must be int"));
                    }
                    let vt = check_expr(
                        v_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if vt != Ty::Int {
                        return Err(serr(v_expr.span(), "set_int value must be int"));
                    }
                    Ok(Ty::Void)
                } else if fname == "get_text" {
                    if args.len() != 2 {
                        return Err(serr(*span, "get_text takes exactly 2 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "get_text does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "get_text does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Ref(Box::new(Ty::Text)) {
                        return Err(serr(p_expr.span(), "get_text expects Addr<text>"));
                    }
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if it != Ty::Int {
                        return Err(serr(i_expr.span(), "get_text index must be int"));
                    }
                    Ok(Ty::Text)
                } else if fname == "set_text" {
                    if args.len() != 3 {
                        return Err(serr(*span, "set_text takes exactly 3 args"));
                    }
                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "set_text does not take named args"));
                        }
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "set_text does not take named args"));
                        }
                    };
                    let v_expr = match &args[2] {
                        Arg::Pos(e) => e,
                        Arg::Named { .. } => {
                            return Err(serr(*span, "set_text does not take named args"));
                        }
                    };
                    let pt = check_expr(
                        p_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if pt != Ty::Ref(Box::new(Ty::Text)) {
                        return Err(serr(p_expr.span(), "set_text expects Addr<text>"));
                    }
                    let it = check_expr(
                        i_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if it != Ty::Int {
                        return Err(serr(i_expr.span(), "set_text index must be int"));
                    }
                    let vt = check_expr(
                        v_expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
                    )?;
                    if vt != Ty::Text {
                        return Err(serr(v_expr.span(), "set_text value must be text"));
                    }
                    Ok(Ty::Void)
                } else {
                    let sig = fns
                        .get(fname)
                        .ok_or_else(|| serr(*sp, format!("unknown function: {fname}")))?;
                    validate_args(
                        args,
                        &sig.params,
                        *span,
                        fn_ret_ty,
                        fns,
                        methods,
                        enums,
                        scopes,
                        structs,
                        module_id,
                    )?;
                    Ok(sig.ret.clone())
                }
            } else {
                Err(serr(*span, "invalid call target"))
            }
        }
    }
}

fn validate_args(
    args: &[Arg],
    expected: &[(String, Ty)],
    span: Span,
    fn_ret_ty: &Ty,
    fns: &HashMap<String, FnSig>,
    methods: &HashMap<String, HashMap<String, MethodSig>>,
    enums: &mut HashMap<String, EnumInfo>,
    scopes: &Vec<HashMap<String, VarInfo>>,
    structs: &HashMap<String, StructInfo>,
    module_id: usize,
) -> Result<(), SemError> {
    if args.iter().all(|a| matches!(a, Arg::Pos(_))) {
        if args.len() != expected.len() {
            return Err(serr(
                span,
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
            let xt = check_expr(
                expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
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
                Arg::Pos(e) => return Err(serr(e.span(), "cannot mix named and positional args")),
            }
        }

        // Check for unknown parameter names
        let param_names: std::collections::HashSet<_> = expected.iter().map(|p| &p.0).collect();
        for (name, (_, arg_span)) in &named {
            if !param_names.contains(name) {
                return Err(serr(*arg_span, format!("unknown parameter: '{}'", name)));
            }
        }

        if named.len() != expected.len() {
            return Err(serr(
                span,
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
                .ok_or_else(|| serr(span, format!("missing parameter: {pname}")))?;
            let ety = check_expr(
                expr, fn_ret_ty, fns, methods, scopes, enums, structs, module_id,
            )?;
            if ety != *pty {
                return Err(serr(*arg_sp, "arg type mismatch"));
            }
        }
        Ok(())
    }
}
