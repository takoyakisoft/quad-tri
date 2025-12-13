use crate::ast::*;
use crate::lex::Span;
use crate::sem::{SemInfo, Ty};
use cranelift::prelude::*;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::HashMap;

#[derive(Debug)]
pub struct EmitError {
    pub span: Span,
    pub msg: String,
}
impl std::fmt::Display for EmitError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.line, self.span.col, self.msg)
    }
}
impl std::error::Error for EmitError {}
fn eerr(span: Span, msg: impl Into<String>) -> EmitError {
    EmitError {
        span,
        msg: msg.into(),
    }
}

pub fn emit_module(prog: &LinkedProgram, sem: &SemInfo) -> Result<Vec<u8>, EmitError> {
    let flag_builder = settings::builder();
    let isa_builder =
        cranelift_native::builder().map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?;

    let object_builder = ObjectBuilder::new(
        isa,
        "quad_module",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?;

    let mut module = ObjectModule::new(object_builder);
    let mut ctx = module.make_context();
    let mut func_ctx = FunctionBuilderContext::new();

    // 1. Collect strings and define data objects
    let mut str_map: HashMap<String, (String, String, usize)> = HashMap::new();
    let mut next_str = 0usize;
    collect_strings(prog, &mut str_map, &mut next_str);

    let mut data_ctx = cranelift_module::DataDescription::new();

    let mut str_data_ids: HashMap<String, cranelift_module::DataId> = HashMap::new();

    for (text, (gname, s_null, _n)) in &str_map {
        data_ctx.define(s_null.as_bytes().to_vec().into_boxed_slice());
        let id = module
            .declare_data(gname, Linkage::Local, true, false)
            .map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?;
        module
            .define_data(id, &data_ctx)
            .map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?;
        data_ctx.clear();
        str_data_ids.insert(text.clone(), id);
    }

    // 2. Declare external functions
    let mut ext_fns: HashMap<String, cranelift_module::FuncId> = HashMap::new();

    // quad_echo_i64
    let mut sig_echo_i64 = module.make_signature();
    sig_echo_i64.params.push(AbiParam::new(types::I64));
    let id_echo_i64 = module
        .declare_function("quad_echo_i64", Linkage::Import, &sig_echo_i64)
        .map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?;
    ext_fns.insert("quad_echo_i64".to_string(), id_echo_i64);

    // quad_echo_cstr
    let mut sig_echo_cstr = module.make_signature();
    sig_echo_cstr
        .params
        .push(AbiParam::new(module.target_config().pointer_type()));
    let id_echo_cstr = module
        .declare_function("quad_echo_cstr", Linkage::Import, &sig_echo_cstr)
        .map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?;
    ext_fns.insert("quad_echo_cstr".to_string(), id_echo_cstr);

    // 3. Declare all user functions first (to handle forward references)
    let mut user_fns: HashMap<String, cranelift_module::FuncId> = HashMap::new();
    for f in &prog.funcs {
        let sig_sem = sem
            .fns
            .get(&f.name)
            .ok_or_else(|| eerr(f.span, "missing sem sig"))?;
        let mut sig_cl = module.make_signature();
        for (_, ty) in &sig_sem.params {
            sig_cl.params.push(AbiParam::new(ty_cl(
                *ty,
                module.target_config().pointer_type(),
            )));
        }
        if sig_sem.ret != Ty::Void {
            sig_cl.returns.push(AbiParam::new(ty_cl(
                sig_sem.ret,
                module.target_config().pointer_type(),
            )));
        }

        let fname = mangle(&f.name);
        // main is exported, others local? For now export all or make main exported.
        // The driver expects `quad_main`.
        let linkage = if f.name == "main" {
            Linkage::Export
        } else {
            Linkage::Local
        };
        let id = module
            .declare_function(&fname, linkage, &sig_cl)
            .map_err(|e| eerr(f.span, e.to_string()))?;
        user_fns.insert(f.name.clone(), id);
    }

    // 4. Define functions
    for f in &prog.funcs {
        let fid = user_fns.get(&f.name).unwrap();
        let sig_sem = sem.fns.get(&f.name).unwrap();

        ctx.func.signature = module.make_signature();
        for (_, ty) in &sig_sem.params {
            ctx.func.signature.params.push(AbiParam::new(ty_cl(
                *ty,
                module.target_config().pointer_type(),
            )));
        }
        if sig_sem.ret != Ty::Void {
            ctx.func.signature.returns.push(AbiParam::new(ty_cl(
                sig_sem.ret,
                module.target_config().pointer_type(),
            )));
        }

        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let mut vars: HashMap<String, (Variable, Ty)> = HashMap::new();
            let mut var_index = 0;

            // Define params as variables
            for (i, (name, ty)) in sig_sem.params.iter().enumerate() {
                let val = builder.block_params(entry_block)[i];
                let var = Variable::new(var_index);
                var_index += 1;
                builder.declare_var(var, ty_cl(*ty, module.target_config().pointer_type()));
                builder.def_var(var, val);
                vars.insert(name.clone(), (var, *ty));
            }

            let ptr_ty = module.target_config().pointer_type();
            let mut comp_ctx = CompilerCtx {
                builder: &mut builder,
                module: &mut module,
                vars: vec![vars],
                next_var: var_index,
                loops: vec![],
                str_data_ids: &str_data_ids,
                ext_fns: &ext_fns,
                user_fns: &user_fns,
                ptr_ty,
                sem,
            };

            for stmt in &f.body {
                compile_stmt(stmt, &mut comp_ctx)?;
            }

            // Fallthrough return for void functions if not present
            if sig_sem.ret == Ty::Void {
                comp_ctx.builder.ins().return_(&[]);
            } else {
                // If non-void function reaches end without return, it's UB or checked by sem.
                // We can insert a trap or dummy return if needed, but sem checks should prevent this.
            }

            builder.finalize();
        }

        module
            .define_function(*fid, &mut ctx)
            .map_err(|e| eerr(f.span, e.to_string()))?;
        module.clear_context(&mut ctx);
    }

    let product = module.finish();
    Ok(product
        .emit()
        .map_err(|e| eerr(Span { line: 0, col: 0 }, e.to_string()))?)
}

struct CompilerCtx<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut ObjectModule,
    vars: Vec<HashMap<String, (Variable, Ty)>>,
    next_var: usize,
    loops: Vec<(Block, Block)>, // (break_block, continue_block)
    str_data_ids: &'a HashMap<String, cranelift_module::DataId>,
    ext_fns: &'a HashMap<String, cranelift_module::FuncId>,
    user_fns: &'a HashMap<String, cranelift_module::FuncId>,
    ptr_ty: Type,
    sem: &'a SemInfo,
}

impl<'a, 'b> CompilerCtx<'a, 'b> {
    fn get_var(&self, name: &str) -> Option<(Variable, Ty)> {
        for scope in self.vars.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(*v);
            }
        }
        None
    }

    fn declare_var(&mut self, name: String, ty: Type, sem_ty: Ty) -> Variable {
        let var = Variable::new(self.next_var);
        self.next_var += 1;
        self.builder.declare_var(var, ty);
        self.vars.last_mut().unwrap().insert(name, (var, sem_ty));
        var
    }
}

fn compile_stmt(stmt: &Stmt, ctx: &mut CompilerCtx) -> Result<(), EmitError> {
    match stmt {
        Stmt::VarDecl { name, ty, init, .. } => {
            let (val, _) = compile_expr(init, ctx)?;
            let sem_ty = parse_ty(ty).unwrap();
            let t = ty_cl(sem_ty, ctx.ptr_ty);
            let var = ctx.declare_var(name.clone(), t, sem_ty);
            ctx.builder.def_var(var, val);
        }
        Stmt::Assign { name, expr, span } => {
            let (val, _) = compile_expr(expr, ctx)?;
            let (var, _) = ctx
                .get_var(name)
                .ok_or_else(|| eerr(*span, "unknown var"))?;
            ctx.builder.def_var(var, val);
        }
        Stmt::Expr { expr, .. } => {
            compile_expr(expr, ctx)?;
        }
        Stmt::Back { expr, .. } => {
            if let Some(e) = expr {
                let (val, _) = compile_expr(e, ctx)?;
                ctx.builder.ins().return_(&[val]);
            } else {
                ctx.builder.ins().return_(&[]);
            }
        }
        Stmt::If {
            arms, else_body, ..
        } => {
            let exit_block = ctx.builder.create_block();

            for (cond, body) in arms {
                let body_block = ctx.builder.create_block();
                let next_cond_block = ctx.builder.create_block();

                let (c, _) = compile_expr(cond, ctx)?;
                ctx.builder
                    .ins()
                    .brif(c, body_block, &[], next_cond_block, &[]);

                ctx.builder.switch_to_block(body_block);
                ctx.builder.seal_block(body_block);

                ctx.vars.push(HashMap::new());
                for s in body {
                    compile_stmt(s, ctx)?;
                }
                ctx.vars.pop();

                if !ctx.builder.is_unreachable() {
                    ctx.builder.ins().jump(exit_block, &[]);
                }

                ctx.builder.switch_to_block(next_cond_block);
                ctx.builder.seal_block(next_cond_block);
            }

            if let Some(eb) = else_body {
                ctx.vars.push(HashMap::new());
                for s in eb {
                    compile_stmt(s, ctx)?;
                }
                ctx.vars.pop();
            }

            if !ctx.builder.is_unreachable() {
                ctx.builder.ins().jump(exit_block, &[]);
            }

            ctx.builder.switch_to_block(exit_block);
            ctx.builder.seal_block(exit_block);
        }
        Stmt::While { cond, body, .. } => {
            let header_block = ctx.builder.create_block();
            let body_block = ctx.builder.create_block();
            let exit_block = ctx.builder.create_block();

            ctx.builder.ins().jump(header_block, &[]);
            ctx.builder.switch_to_block(header_block);

            let (c, _) = compile_expr(cond, ctx)?;
            ctx.builder.ins().brif(c, body_block, &[], exit_block, &[]);

            ctx.builder.switch_to_block(body_block);
            // Loop body
            ctx.loops.push((exit_block, header_block));
            ctx.vars.push(HashMap::new());
            for s in body {
                compile_stmt(s, ctx)?;
            }
            ctx.vars.pop();
            ctx.loops.pop();

            if !ctx.builder.is_unreachable() {
                ctx.builder.ins().jump(header_block, &[]);
            }

            ctx.builder.switch_to_block(exit_block);
            ctx.builder.seal_block(header_block);
            ctx.builder.seal_block(body_block);
            ctx.builder.seal_block(exit_block);
        }
        Stmt::Break { span } => {
            let (brk, _) = ctx
                .loops
                .last()
                .ok_or_else(|| eerr(*span, "break outside loop"))?;
            ctx.builder.ins().jump(*brk, &[]);
        }
        Stmt::Continue { span } => {
            let (_, cnt) = ctx
                .loops
                .last()
                .ok_or_else(|| eerr(*span, "continue outside loop"))?;
            ctx.builder.ins().jump(*cnt, &[]);
        }
    }
    Ok(())
}

fn compile_expr(expr: &Expr, ctx: &mut CompilerCtx) -> Result<(Value, Ty), EmitError> {
    match expr {
        Expr::Int(n, _) => Ok((ctx.builder.ins().iconst(types::I64, *n), Ty::Int)),
        Expr::Str(s, _) => {
            let data_id = ctx.str_data_ids.get(s).unwrap();
            let global_val = ctx.module.declare_data_in_func(*data_id, ctx.builder.func);
            let ptr = ctx.builder.ins().global_value(ctx.ptr_ty, global_val);
            Ok((ptr, Ty::Text))
        }
        Expr::Ident(name, span) => {
            let (var, ty) = ctx
                .get_var(name)
                .ok_or_else(|| eerr(*span, "unknown var"))?;
            Ok((ctx.builder.use_var(var), ty))
        }
        Expr::BuiltinPrint(_) => panic!("builtin print used as value"), // Should be handled in Call
        Expr::Unary { op, expr, .. } => {
            let (val, ty) = compile_expr(expr, ctx)?;
            match op {
                UnOp::Neg => Ok((ctx.builder.ins().ineg(val), ty)),
                UnOp::Not => Ok((ctx.builder.ins().bxor_imm(val, 1), ty)), // Logical not for 0/1 bools
            }
        }
        Expr::Binary { op, lhs, rhs, .. } => {
            let (l, lty) = compile_expr(lhs, ctx)?;
            let (r, _) = compile_expr(rhs, ctx)?;
            match op {
                BinOp::Add => Ok((ctx.builder.ins().iadd(l, r), lty)),
                BinOp::Sub => Ok((ctx.builder.ins().isub(l, r), lty)),
                BinOp::Mul => Ok((ctx.builder.ins().imul(l, r), lty)),
                BinOp::Div => Ok((ctx.builder.ins().sdiv(l, r), lty)), // Signed div
                BinOp::Mod => Ok((ctx.builder.ins().srem(l, r), lty)),
                BinOp::Shl => Ok((ctx.builder.ins().ishl(l, r), lty)),
                BinOp::Shr => Ok((ctx.builder.ins().sshr(l, r), lty)), // Arithmetic shift right
                BinOp::BitAnd | BinOp::And => Ok((ctx.builder.ins().band(l, r), lty)),
                BinOp::BitOr | BinOp::Or => Ok((ctx.builder.ins().bor(l, r), lty)),
                BinOp::BitXor => Ok((ctx.builder.ins().bxor(l, r), lty)),
                BinOp::Eq => {
                    let cmp = ctx.builder.ins().icmp(IntCC::Equal, l, r);
                    Ok((cmp, Ty::Bool))
                }
                BinOp::Ne => {
                    let cmp = ctx.builder.ins().icmp(IntCC::NotEqual, l, r);
                    Ok((cmp, Ty::Bool))
                }
                BinOp::Lt => {
                    let cmp = ctx.builder.ins().icmp(IntCC::SignedLessThan, l, r);
                    Ok((cmp, Ty::Bool))
                }
                BinOp::Le => {
                    let cmp = ctx.builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r);
                    Ok((cmp, Ty::Bool))
                }
                BinOp::Gt => {
                    let cmp = ctx.builder.ins().icmp(IntCC::SignedGreaterThan, l, r);
                    Ok((cmp, Ty::Bool))
                }
                BinOp::Ge => {
                    let cmp = ctx
                        .builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, l, r);
                    Ok((cmp, Ty::Bool))
                }
            }
        }
        Expr::Call { callee, args, span } => {
            if matches!(&**callee, Expr::BuiltinPrint(_)) {
                // echo
                let arg = &args[0];
                let (val, ty) = match arg {
                    Arg::Pos(e) => compile_expr(e, ctx)?,
                    _ => unreachable!(),
                };

                if ty == Ty::Int {
                    let fid = ctx.ext_fns.get("quad_echo_i64").unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(func_ref, &[val]);
                } else {
                    let fid = ctx.ext_fns.get("quad_echo_cstr").unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(func_ref, &[val]);
                }
                Ok((ctx.builder.ins().iconst(types::I64, 0), Ty::Void)) // Void return dummy
            } else if let Expr::Ident(fname, _) = &**callee {
                let fid = ctx
                    .user_fns
                    .get(fname)
                    .ok_or_else(|| eerr(*span, "unknown func"))?;
                let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);

                let mut arg_vals = Vec::new();
                // Handle named/pos args. Sem check ensures correctness.
                // We need to map args to the order in signature.
                let sig = ctx.sem.fns.get(fname).unwrap();

                // Simple case: all positional
                if args.iter().all(|a| matches!(a, Arg::Pos(_))) {
                    for a in args {
                        if let Arg::Pos(e) = a {
                            let (v, _) = compile_expr(e, ctx)?;
                            arg_vals.push(v);
                        }
                    }
                } else {
                    // Named args
                    let mut named_map = HashMap::new();
                    for a in args {
                        if let Arg::Named { name, expr, .. } = a {
                            named_map.insert(name.clone(), expr);
                        }
                    }
                    for (pname, _) in &sig.params {
                        let e = named_map.get(pname).unwrap();
                        let (v, _) = compile_expr(e, ctx)?;
                        arg_vals.push(v);
                    }
                }

                let call = ctx.builder.ins().call(func_ref, &arg_vals);
                let results = ctx.builder.inst_results(call);
                if results.is_empty() {
                    Ok((ctx.builder.ins().iconst(types::I64, 0), Ty::Void)) // Void
                } else {
                    Ok((results[0], sig.ret))
                }
            } else {
                Err(eerr(*span, "invalid call"))
            }
        }
    }
}

fn ty_cl(t: Ty, ptr_ty: Type) -> Type {
    match t {
        Ty::Int => types::I64,
        Ty::Bool => types::I8,
        Ty::Text => ptr_ty,
        Ty::Void => types::I64, // Should not happen in params/ret usually, handled separately
    }
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

fn mangle(name: &str) -> String {
    format!("quad_{name}")
}

fn collect_strings(
    prog: &LinkedProgram,
    map: &mut HashMap<String, (String, String, usize)>,
    next: &mut usize,
) {
    for f in &prog.funcs {
        for s in &f.body {
            collect_strings_stmt(s, map, next);
        }
    }
}

fn collect_strings_stmt(
    stmt: &Stmt,
    map: &mut HashMap<String, (String, String, usize)>,
    next: &mut usize,
) {
    match stmt {
        Stmt::VarDecl { init, .. } => collect_strings_expr(init, map, next),
        Stmt::Assign { expr, .. } => collect_strings_expr(expr, map, next),
        Stmt::Expr { expr, .. } => collect_strings_expr(expr, map, next),
        Stmt::Back { expr, .. } => {
            if let Some(e) = expr {
                collect_strings_expr(e, map, next)
            }
        }
        Stmt::If {
            arms, else_body, ..
        } => {
            for (c, b) in arms {
                collect_strings_expr(c, map, next);
                for s in b {
                    collect_strings_stmt(s, map, next);
                }
            }
            if let Some(b) = else_body {
                for s in b {
                    collect_strings_stmt(s, map, next);
                }
            }
        }
        Stmt::While { cond, body, .. } => {
            collect_strings_expr(cond, map, next);
            for s in body {
                collect_strings_stmt(s, map, next);
            }
        }
        _ => {}
    }
}

fn collect_strings_expr(
    expr: &Expr,
    map: &mut HashMap<String, (String, String, usize)>,
    next: &mut usize,
) {
    match expr {
        Expr::Str(s, _) => {
            if !map.contains_key(s) {
                let gname = format!("str_{}", next);
                *next += 1;
                // Append null terminator for C compatibility
                let mut s_null = s.clone();
                s_null.push('\0');
                let len = s_null.len();
                map.insert(s.clone(), (gname, s_null, len));
            }
        }
        Expr::Unary { expr, .. } => collect_strings_expr(expr, map, next),
        Expr::Binary { lhs, rhs, .. } => {
            collect_strings_expr(lhs, map, next);
            collect_strings_expr(rhs, map, next);
        }
        Expr::Call { args, .. } => {
            for a in args {
                match a {
                    Arg::Pos(e) => collect_strings_expr(e, map, next),
                    Arg::Named { expr, .. } => collect_strings_expr(expr, map, next),
                }
            }
        }
        _ => {}
    }
}
