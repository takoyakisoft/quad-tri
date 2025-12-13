use crate::ast::*;
use crate::lex::Span;
use crate::sem::{SemInfo, Ty};
use cranelift::prelude::*;
use cranelift_codegen::ir::StackSlot;
use cranelift_module::{Linkage, Module};
use cranelift_object::{ObjectBuilder, ObjectModule};
use std::collections::{HashMap, HashSet};

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
                ty.clone(),
                module.target_config().pointer_type(),
            )));
        }
        if sig_sem.ret != Ty::Void {
            sig_cl.returns.push(AbiParam::new(ty_cl(
                sig_sem.ret.clone(),
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
                ty.clone(),
                module.target_config().pointer_type(),
            )));
        }
        if sig_sem.ret != Ty::Void {
            ctx.func.signature.returns.push(AbiParam::new(ty_cl(
                sig_sem.ret.clone(),
                module.target_config().pointer_type(),
            )));
        }

        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let mut vars: HashMap<String, VarBinding> = HashMap::new();
            let mut var_index = 0;

            // Define params as variables
            for (i, (name, ty)) in sig_sem.params.iter().enumerate() {
                let val = builder.block_params(entry_block)[i];
                match ty {
                    Ty::Struct(sname) => {
                        vars.insert(
                            name.clone(),
                            VarBinding::StructRef {
                                addr: val,
                                name: sname.clone(),
                            },
                        );
                    }
                    _ => {
                        let var = Variable::new(var_index);
                        var_index += 1;
                        builder.declare_var(
                            var,
                            ty_cl(ty.clone(), module.target_config().pointer_type()),
                        );
                        builder.def_var(var, val);
                        vars.insert(
                            name.clone(),
                            VarBinding::Scalar {
                                var,
                                ty: ty.clone(),
                            },
                        );
                    }
                }
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
                layouts: HashMap::new(),
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
    vars: Vec<HashMap<String, VarBinding>>,
    next_var: usize,
    loops: Vec<(Block, Block)>, // (break_block, continue_block)
    str_data_ids: &'a HashMap<String, cranelift_module::DataId>,
    ext_fns: &'a HashMap<String, cranelift_module::FuncId>,
    user_fns: &'a HashMap<String, cranelift_module::FuncId>,
    ptr_ty: Type,
    sem: &'a SemInfo,
    layouts: HashMap<String, StructLayout>,
}

impl<'a, 'b> CompilerCtx<'a, 'b> {
    fn get_var(&self, name: &str) -> Option<VarBinding> {
        for scope in self.vars.iter().rev() {
            if let Some(v) = scope.get(name) {
                return Some(v.clone());
            }
        }
        None
    }

    fn declare_scalar(&mut self, name: String, ty: Type, sem_ty: Ty) -> Variable {
        let var = Variable::new(self.next_var);
        self.next_var += 1;
        self.builder.declare_var(var, ty);
        self.vars
            .last_mut()
            .unwrap()
            .insert(name, VarBinding::Scalar { var, ty: sem_ty });
        var
    }
}

#[derive(Clone)]
enum VarBinding {
    Scalar { var: Variable, ty: Ty },
    StructOwned { slot: StackSlot, name: String },
    StructRef { addr: Value, name: String },
}

#[derive(Clone)]
struct StructLayout {
    size: i32,
    align: i32,
    fields: Vec<FieldLayout>,
}

#[derive(Clone)]
struct FieldLayout {
    name: String,
    offset: i32,
    ty: Ty,
}

#[derive(Clone)]
enum ValueKind {
    Scalar(Value, Ty),
    StructPtr {
        addr: Value,
        ty_name: String,
        slot: Option<StackSlot>,
    },
}

fn compile_stmt(stmt: &Stmt, ctx: &mut CompilerCtx) -> Result<(), EmitError> {
    match stmt {
        Stmt::VarDecl { name, ty, init, .. } => {
            let init_val = compile_expr(init, ctx)?;
            let sem_ty = crate::sem::parse_ty(ty, &ctx.sem.structs).unwrap();
            match sem_ty {
                Ty::Struct(ref sname) => {
                    let layout = get_struct_layout(sname, ctx, init.span())?;
                    let slot = match init_val {
                        ValueKind::StructPtr { slot: Some(s), .. } => s,
                        ValueKind::StructPtr { addr, .. } => {
                            let align_shift = layout.align.trailing_zeros() as u8;
                            let new_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                layout.size as u32,
                                align_shift,
                            ));
                            let dst = ctx.builder.ins().stack_addr(ctx.ptr_ty, new_slot, 0);
                            copy_struct(addr, dst, &layout, ctx)?;
                            new_slot
                        }
                        ValueKind::Scalar(_, _) => {
                            return Err(eerr(init.span(), "expected struct value"));
                        }
                    };
                    ctx.vars.last_mut().unwrap().insert(
                        name.clone(),
                        VarBinding::StructOwned {
                            slot,
                            name: sname.clone(),
                        },
                    );
                }
                _ => {
                    let (val, vty) = match init_val {
                        ValueKind::Scalar(v, ty) => (v, ty),
                        ValueKind::StructPtr { .. } => {
                            return Err(eerr(init.span(), "expected scalar value"))
                        }
                    };
                    if vty != sem_ty {
                        return Err(eerr(init.span(), "type mismatch in var decl"));
                    }
                    let t = ty_cl(sem_ty.clone(), ctx.ptr_ty);
                    let var = ctx.declare_scalar(name.clone(), t, sem_ty);
                    ctx.builder.def_var(var, val);
                }
            }
        }
        Stmt::Assign { target, expr, span } => {
            let val = compile_expr(expr, ctx)?;
            match target {
                AssignTarget::Name(name) => {
                    let binding = ctx
                        .get_var(name)
                        .ok_or_else(|| eerr(*span, "unknown var"))?;
                    match binding {
                        VarBinding::Scalar { var, ty } => {
                            let (v, vty) = match val {
                                ValueKind::Scalar(v, t) => (v, t),
                                ValueKind::StructPtr { .. } => {
                                    return Err(eerr(*span, "type mismatch in assignment"))
                                }
                            };
                            if vty != ty {
                                return Err(eerr(*span, "type mismatch in assignment"));
                            }
                            ctx.builder.def_var(var, v);
                        }
                        VarBinding::StructOwned { slot, name: sname } => {
                            let src_addr = match val {
                                ValueKind::StructPtr { addr, ty_name, .. } => {
                                    if ty_name != sname {
                                        return Err(eerr(*span, "struct type mismatch"));
                                    }
                                    addr
                                }
                                _ => return Err(eerr(*span, "type mismatch in assignment")),
                            };
                            let layout = get_struct_layout(&sname, ctx, *span)?;
                            let dst_addr = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);
                            copy_struct(src_addr, dst_addr, &layout, ctx)?;
                        }
                        VarBinding::StructRef { addr, name: sname } => {
                            let src_addr = match val {
                                ValueKind::StructPtr {
                                    addr: a, ty_name, ..
                                } => {
                                    if ty_name != sname {
                                        return Err(eerr(*span, "struct type mismatch"));
                                    }
                                    a
                                }
                                _ => return Err(eerr(*span, "type mismatch in assignment")),
                            };
                            let layout = get_struct_layout(&sname, ctx, *span)?;
                            copy_struct(src_addr, addr, &layout, ctx)?;
                        }
                    }
                }
                AssignTarget::Field { base, field } => {
                    let binding = ctx
                        .get_var(base)
                        .ok_or_else(|| eerr(*span, "unknown var"))?;
                    let (sname, addr) = match binding {
                        VarBinding::StructOwned { slot, name } => {
                            (name, ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0))
                        }
                        VarBinding::StructRef { addr, name } => (name, addr),
                        VarBinding::Scalar { .. } => {
                            return Err(eerr(*span, "field assign on non-struct"))
                        }
                    };
                    let layout = get_struct_layout(&sname, ctx, *span)?;
                    let fld = layout
                        .fields
                        .iter()
                        .find(|f| f.name.as_str() == field.as_str())
                        .ok_or_else(|| eerr(*span, "unknown field"))?;
                    let (v, vty) = match val {
                        ValueKind::Scalar(v, ty) => (v, ty),
                        ValueKind::StructPtr { .. } => {
                            return Err(eerr(*span, "type mismatch in field assignment"))
                        }
                    };
                    if vty != fld.ty {
                        return Err(eerr(*span, "type mismatch in field assignment"));
                    }
                    store_field(addr, fld.offset, v, &fld.ty, ctx)?;
                }
            }
        }
        Stmt::Expr { expr, .. } => {
            let _ = compile_expr(expr, ctx)?;
        }
        Stmt::Back { expr, .. } => {
            if let Some(e) = expr {
                let val = match compile_expr(e, ctx)? {
                    ValueKind::Scalar(v, _) => v,
                    ValueKind::StructPtr { addr, .. } => addr,
                };

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

                let (c, _) = expect_scalar(compile_expr(cond, ctx)?, cond.span())?;
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

            let (c, _) = expect_scalar(compile_expr(cond, ctx)?, cond.span())?;
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

fn compile_expr(expr: &Expr, ctx: &mut CompilerCtx) -> Result<ValueKind, EmitError> {
    match expr {
        Expr::Int(n, _) => Ok(ValueKind::Scalar(
            ctx.builder.ins().iconst(types::I64, *n),
            Ty::Int,
        )),
        Expr::Str(s, _) => {
            let data_id = ctx.str_data_ids.get(s).unwrap();
            let global_val = ctx.module.declare_data_in_func(*data_id, ctx.builder.func);
            let ptr = ctx.builder.ins().global_value(ctx.ptr_ty, global_val);
            Ok(ValueKind::Scalar(ptr, Ty::Text))
        }
        Expr::Ident(name, span) => match ctx.get_var(name) {
            Some(VarBinding::Scalar { var, ty }) => {
                Ok(ValueKind::Scalar(ctx.builder.use_var(var), ty))
            }
            Some(VarBinding::StructOwned { slot, name }) => Ok(ValueKind::StructPtr {
                addr: ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0),
                ty_name: name,
                slot: Some(slot),
            }),
            Some(VarBinding::StructRef { addr, name }) => Ok(ValueKind::StructPtr {
                addr,
                ty_name: name,
                slot: None,
            }),
            None => Err(eerr(*span, "unknown var")),
        },
        Expr::StructLit { name, fields, span } => {
            let layout = get_struct_layout(name, ctx, *span)?;
            let align_shift = layout.align.trailing_zeros() as u8;
            let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                layout.size as u32,
                align_shift,
            ));
            let base = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);
            for fld in &layout.fields {
                let (_, expr, fspan) = fields
                    .iter()
                    .find(|(n, _, _)| *n == fld.name)
                    .ok_or_else(|| eerr(*span, "missing field"))?;
                let (v, vty) = match compile_expr(expr, ctx)? {
                    ValueKind::Scalar(v, ty) => (v, ty),
                    ValueKind::StructPtr { .. } => {
                        return Err(eerr(*fspan, "nested struct literals not supported"))
                    }
                };
                if vty != fld.ty {
                    return Err(eerr(*fspan, "field type mismatch"));
                }
                store_field(base, fld.offset, v, &fld.ty, ctx)?;
            }
            Ok(ValueKind::StructPtr {
                addr: base,
                ty_name: name.clone(),
                slot: Some(slot),
            })
        }
        Expr::Field { base, field, span } => match compile_expr(base, ctx)? {
            ValueKind::StructPtr { addr, ty_name, .. } => {
                let layout = get_struct_layout(&ty_name, ctx, *span)?;
                let fld = layout
                    .fields
                    .iter()
                    .find(|f| f.name.as_str() == field.as_str())
                    .ok_or_else(|| eerr(*span, "unknown field"))?;
                let val = load_field(addr, fld.offset, &fld.ty, ctx)?;
                Ok(ValueKind::Scalar(val, fld.ty.clone()))
            }
            ValueKind::Scalar(_, _) => Err(eerr(*span, "field access on non-struct")),
        },
        Expr::BuiltinPrint(span) => {
            return Err(eerr(*span, "builtin print cannot be used as a value"))
        }
        Expr::Unary { op, expr, span } => {
            let (val, ty) = expect_scalar(compile_expr(expr, ctx)?, *span)?;
            let v = match op {
                UnOp::Neg => ctx.builder.ins().ineg(val),
                UnOp::Not => ctx.builder.ins().bxor_imm(val, 1),
            };
            Ok(ValueKind::Scalar(v, ty))
        }
        Expr::Binary { op, lhs, rhs, span } => {
            let (l, lty) = expect_scalar(compile_expr(lhs, ctx)?, *span)?;
            let (r, _) = expect_scalar(compile_expr(rhs, ctx)?, *span)?;
            let (res, ty) = match op {
                BinOp::Add => (ctx.builder.ins().iadd(l, r), lty.clone()),
                BinOp::Sub => (ctx.builder.ins().isub(l, r), lty.clone()),
                BinOp::Mul => (ctx.builder.ins().imul(l, r), lty.clone()),
                BinOp::Div => (ctx.builder.ins().sdiv(l, r), lty.clone()),
                BinOp::Mod => (ctx.builder.ins().srem(l, r), lty.clone()),
                BinOp::Shl => (ctx.builder.ins().ishl(l, r), lty.clone()),
                BinOp::Shr => (ctx.builder.ins().sshr(l, r), lty.clone()),
                BinOp::BitAnd | BinOp::And => (ctx.builder.ins().band(l, r), lty.clone()),
                BinOp::BitOr | BinOp::Or => (ctx.builder.ins().bor(l, r), lty.clone()),
                BinOp::BitXor => (ctx.builder.ins().bxor(l, r), lty.clone()),
                BinOp::Eq => (ctx.builder.ins().icmp(IntCC::Equal, l, r), Ty::Bool),
                BinOp::Ne => (ctx.builder.ins().icmp(IntCC::NotEqual, l, r), Ty::Bool),
                BinOp::Lt => (
                    ctx.builder.ins().icmp(IntCC::SignedLessThan, l, r),
                    Ty::Bool,
                ),
                BinOp::Le => (
                    ctx.builder.ins().icmp(IntCC::SignedLessThanOrEqual, l, r),
                    Ty::Bool,
                ),
                BinOp::Gt => (
                    ctx.builder.ins().icmp(IntCC::SignedGreaterThan, l, r),
                    Ty::Bool,
                ),
                BinOp::Ge => (
                    ctx.builder
                        .ins()
                        .icmp(IntCC::SignedGreaterThanOrEqual, l, r),
                    Ty::Bool,
                ),
            };
            Ok(ValueKind::Scalar(res, ty))
        }
        Expr::Call { callee, args, span } => {
            if matches!(&**callee, Expr::BuiltinPrint(_)) {
                let arg = &args[0];
                let (val, ty) = match arg {
                    Arg::Pos(e) => expect_scalar(compile_expr(e, ctx)?, *span)?,
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
                Ok(ValueKind::Scalar(
                    ctx.builder.ins().iconst(types::I64, 0),
                    Ty::Void,
                ))
            } else if let Expr::Field {
                base,
                field,
                span: field_span,
            } = &**callee
            {
                if let Expr::Ident(type_name, _) = &**base {
                    let msig = ctx
                        .sem
                        .methods
                        .get(type_name)
                        .and_then(|m| m.get(field))
                        .ok_or_else(|| eerr(*field_span, "unknown method"))?;
                    if msig.has_self {
                        return Err(eerr(*field_span, "method expects self receiver"));
                    }

                    let fid = ctx
                        .user_fns
                        .get(&msig.symbol)
                        .ok_or_else(|| eerr(*field_span, "missing method symbol"))?;
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);

                    let arg_vals = build_call_args(args, &msig.params, *span, ctx)?;
                    let call = ctx.builder.ins().call(func_ref, &arg_vals);
                    let results = ctx.builder.inst_results(call);
                    if results.is_empty() {
                        Ok(ValueKind::Scalar(
                            ctx.builder.ins().iconst(types::I64, 0),
                            Ty::Void,
                        ))
                    } else if let Ty::Struct(name) = &msig.ret {
                        Ok(ValueKind::StructPtr {
                            addr: results[0],
                            ty_name: name.clone(),
                            slot: None,
                        })
                    } else {
                        Ok(ValueKind::Scalar(results[0], msig.ret.clone()))
                    }
                } else {
                    let recv_val = compile_expr(base, ctx)?;
                    let (sname, recv_ptr) = match recv_val {
                        ValueKind::StructPtr { addr, ty_name, .. } => (ty_name, addr),
                        ValueKind::Scalar(_, _) => {
                            return Err(eerr(*field_span, "method call requires struct receiver"))
                        }
                    };
                    let msig = ctx
                        .sem
                        .methods
                        .get(&sname)
                        .and_then(|m| m.get(field))
                        .ok_or_else(|| eerr(*field_span, "unknown method"))?;
                    if !msig.has_self {
                        return Err(eerr(*field_span, "associated function called as method"));
                    }
                    let fid = ctx
                        .user_fns
                        .get(&msig.symbol)
                        .ok_or_else(|| eerr(*field_span, "missing method symbol"))?;
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);

                    let mut arg_vals = vec![recv_ptr];
                    let mut rest = build_call_args(args, &msig.params[1..], *span, ctx)?;
                    arg_vals.append(&mut rest);

                    let call = ctx.builder.ins().call(func_ref, &arg_vals);
                    let results = ctx.builder.inst_results(call);
                    if results.is_empty() {
                        Ok(ValueKind::Scalar(
                            ctx.builder.ins().iconst(types::I64, 0),
                            Ty::Void,
                        ))
                    } else if let Ty::Struct(name) = &msig.ret {
                        Ok(ValueKind::StructPtr {
                            addr: results[0],
                            ty_name: name.clone(),
                            slot: None,
                        })
                    } else {
                        Ok(ValueKind::Scalar(results[0], msig.ret.clone()))
                    }
                }
            } else if let Expr::Ident(fname, _) = &**callee {
                let fid = ctx
                    .user_fns
                    .get(fname)
                    .ok_or_else(|| eerr(*span, "unknown func"))?;
                let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);

                let sig = ctx.sem.fns.get(fname).unwrap();
                let arg_vals = build_call_args(args, &sig.params, *span, ctx)?;

                let call = ctx.builder.ins().call(func_ref, &arg_vals);
                let results = ctx.builder.inst_results(call);
                if results.is_empty() {
                    Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ))
                } else if let Ty::Struct(name) = &sig.ret {
                    Ok(ValueKind::StructPtr {
                        addr: results[0],
                        ty_name: name.clone(),
                        slot: None,
                    })
                } else {
                    Ok(ValueKind::Scalar(results[0], sig.ret.clone()))
                }
            } else {
                Err(eerr(*span, "invalid call"))
            }
        }
    }
}

fn build_call_args(
    args: &[Arg],
    expected: &[(String, Ty)],
    span: Span,
    ctx: &mut CompilerCtx,
) -> Result<Vec<Value>, EmitError> {
    let mut arg_exprs: Vec<(&Expr, Span)> = Vec::new();
    if args.iter().all(|a| matches!(a, Arg::Pos(_))) {
        if args.len() != expected.len() {
            return Err(eerr(
                span,
                format!(
                    "arg count mismatch: expected {}, got {}",
                    expected.len(),
                    args.len()
                ),
            ));
        }
        for a in args {
            if let Arg::Pos(e) = a {
                arg_exprs.push((e, e.span()));
            }
        }
    } else {
        let mut named_map = HashMap::<String, (&Expr, Span)>::new();
        for a in args {
            match a {
                Arg::Named { name, expr, span } => {
                    if named_map.insert(name.clone(), (expr, *span)).is_some() {
                        return Err(eerr(*span, "duplicate named arg"));
                    }
                }
                Arg::Pos(e) => return Err(eerr(e.span(), "cannot mix named and positional args")),
            }
        }

        let param_names: HashSet<_> = expected.iter().map(|(n, _)| n.clone()).collect();
        for k in named_map.keys() {
            if !param_names.contains(k) {
                return Err(eerr(span, format!("unknown parameter: {k}")));
            }
        }
        if named_map.len() != expected.len() {
            return Err(eerr(
                span,
                format!(
                    "argument count mismatch for named call: expected {}, got {}",
                    expected.len(),
                    named_map.len()
                ),
            ));
        }
        for (pname, _) in expected {
            let (expr, arg_sp) = named_map
                .get(pname)
                .ok_or_else(|| eerr(span, format!("missing parameter: {pname}")))?;
            arg_exprs.push((expr, *arg_sp));
        }
    }

    let mut arg_vals = Vec::new();
    for (i, (expr_ref, arg_sp)) in arg_exprs.iter().enumerate() {
        let expected_ty = &expected[i].1;
        match expected_ty {
            Ty::Struct(sname) => {
                let ptr = match compile_expr(expr_ref, ctx)? {
                    ValueKind::StructPtr { addr, ty_name, .. } => {
                        if &ty_name != sname {
                            return Err(eerr(*arg_sp, "struct arg type mismatch"));
                        }
                        addr
                    }
                    ValueKind::Scalar(_, _) => return Err(eerr(*arg_sp, "expected struct arg")),
                };
                arg_vals.push(ptr);
            }
            _ => {
                let (v, vty) = expect_scalar(compile_expr(expr_ref, ctx)?, *arg_sp)?;
                if &vty != expected_ty {
                    return Err(eerr(*arg_sp, "arg type mismatch"));
                }
                arg_vals.push(v);
            }
        }
    }

    Ok(arg_vals)
}

fn expect_scalar(v: ValueKind, span: Span) -> Result<(Value, Ty), EmitError> {
    match v {
        ValueKind::Scalar(v, ty) => Ok((v, ty)),
        ValueKind::StructPtr { .. } => Err(eerr(span, "expected scalar value")),
    }
}

fn ty_cl(t: Ty, ptr_ty: Type) -> Type {
    match t {
        Ty::Int => types::I64,
        Ty::Bool => types::I8,
        Ty::Text => ptr_ty,
        Ty::Struct(_) => ptr_ty,
        Ty::Void => types::I64, // Should not happen in params/ret usually, handled separately
    }
}

fn load_field(
    base: Value,
    offset: i32,
    ty: &Ty,
    ctx: &mut CompilerCtx,
) -> Result<Value, EmitError> {
    let cl_ty = ty_cl(ty.clone(), ctx.ptr_ty);
    Ok(ctx.builder.ins().load(cl_ty, MemFlags::new(), base, offset))
}

fn store_field(
    base: Value,
    offset: i32,
    val: Value,
    ty: &Ty,
    ctx: &mut CompilerCtx,
) -> Result<(), EmitError> {
    ctx.builder.ins().store(MemFlags::new(), val, base, offset);
    let _ = ty;
    Ok(())
}

fn copy_struct(
    src: Value,
    dst: Value,
    layout: &StructLayout,
    ctx: &mut CompilerCtx,
) -> Result<(), EmitError> {
    for fld in &layout.fields {
        let v = load_field(src, fld.offset, &fld.ty, ctx)?;
        store_field(dst, fld.offset, v, &fld.ty, ctx)?;
    }
    Ok(())
}

fn get_struct_layout(
    name: &str,
    ctx: &mut CompilerCtx,
    span: Span,
) -> Result<StructLayout, EmitError> {
    if let Some(l) = ctx.layouts.get(name) {
        return Ok(l.clone());
    }
    let sinfo = ctx
        .sem
        .structs
        .get(name)
        .ok_or_else(|| eerr(span, "unknown struct"))?;
    let mut offset = 0i32;
    let mut align = 1i32;
    let mut fields = Vec::new();
    for (fname, fty) in &sinfo.fields {
        let (sz, al) = ty_size_align(fty, ctx, span)?;
        offset = align_up(offset, al);
        align = align.max(al);
        fields.push(FieldLayout {
            name: fname.clone(),
            offset,
            ty: fty.clone(),
        });
        offset += sz;
    }
    let size = align_up(offset, align);
    let layout = StructLayout {
        size,
        align,
        fields,
    };
    ctx.layouts.insert(name.to_string(), layout.clone());
    Ok(layout)
}

fn ty_size_align(ty: &Ty, ctx: &mut CompilerCtx, span: Span) -> Result<(i32, i32), EmitError> {
    let ptr_bytes = ctx.module.target_config().pointer_bytes() as i32;
    Ok(match ty {
        Ty::Int => (8, 8),
        Ty::Bool => (1, 1),
        Ty::Text | Ty::Struct(_) => (ptr_bytes, ptr_bytes),
        Ty::Void => return Err(eerr(span, "void has no size")),
    })
}

fn align_up(n: i32, align: i32) -> i32 {
    if align <= 1 {
        return n;
    }
    let rem = n % align;
    if rem == 0 {
        n
    } else {
        n + (align - rem)
    }
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
        Expr::Field { base, .. } => collect_strings_expr(base, map, next),
        Expr::StructLit { fields, .. } => {
            for (_, e, _) in fields {
                collect_strings_expr(e, map, next);
            }
        }
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
