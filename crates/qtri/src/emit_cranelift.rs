use crate::ast::*;
use crate::lex::Span;
use crate::sem::{SemInfo, Ty};
use cranelift::prelude::*;
use cranelift_codegen::ir::{BlockArg, Opcode, StackSlot};
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
    let isa_builder = cranelift_native::builder().map_err(|e| {
        eerr(
            Span {
                file_id: 0,
                line: 0,
                col: 0,
            },
            e.to_string(),
        )
    })?;
    let isa = isa_builder
        .finish(settings::Flags::new(flag_builder))
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;

    let object_builder = ObjectBuilder::new(
        isa,
        "quad_module",
        cranelift_module::default_libcall_names(),
    )
    .map_err(|e| {
        eerr(
            Span {
                file_id: 0,
                line: 0,
                col: 0,
            },
            e.to_string(),
        )
    })?;

    let mut module = ObjectModule::new(object_builder);
    let mut ctx = module.make_context();
    let mut func_ctx = FunctionBuilderContext::new();

    // 1. Collect strings and define data objects
    let mut str_map: HashMap<String, (String, String, usize)> = HashMap::new();
    let mut next_str = 0usize;
    collect_strings(prog, &mut str_map, &mut next_str);

    // Builtin strings used by compiler-generated panics.
    if !str_map.contains_key("unwrap failed") {
        let gname = format!("str_{}", next_str);
        let mut s_null = "unwrap failed".to_string();
        s_null.push('\0');
        let len = s_null.len();
        str_map.insert("unwrap failed".to_string(), (gname, s_null, len));
    }

    let mut data_ctx = cranelift_module::DataDescription::new();

    let mut str_data_ids: HashMap<String, cranelift_module::DataId> = HashMap::new();

    for (text, (gname, s_null, _n)) in &str_map {
        data_ctx.define(s_null.as_bytes().to_vec().into_boxed_slice());
        let id = module
            .declare_data(gname, Linkage::Local, true, false)
            .map_err(|e| {
                eerr(
                    Span {
                        file_id: 0,
                        line: 0,
                        col: 0,
                    },
                    e.to_string(),
                )
            })?;
        module.define_data(id, &data_ctx).map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
        data_ctx.clear();
        str_data_ids.insert(text.clone(), id);
    }

    // 2. Declare external functions
    let mut ext_fns: HashMap<String, cranelift_module::FuncId> = HashMap::new();

    // qtrt_echo_i64
    let mut sig_echo_i64 = module.make_signature();
    sig_echo_i64.params.push(AbiParam::new(types::I64));
    let id_echo_i64 = module
        .declare_function("qtrt_echo_i64", Linkage::Import, &sig_echo_i64)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_echo_i64".to_string(), id_echo_i64);

    // qtrt_print_i64
    let mut sig_print_i64 = module.make_signature();
    sig_print_i64.params.push(AbiParam::new(types::I64));
    let id_print_i64 = module
        .declare_function("qtrt_print_i64", Linkage::Import, &sig_print_i64)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_print_i64".to_string(), id_print_i64);

    // qtrt_echo_cstr
    let mut sig_echo_cstr = module.make_signature();
    sig_echo_cstr
        .params
        .push(AbiParam::new(module.target_config().pointer_type()));
    let id_echo_cstr = module
        .declare_function("qtrt_echo_cstr", Linkage::Import, &sig_echo_cstr)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_echo_cstr".to_string(), id_echo_cstr);

    // qtrt_print_cstr
    let mut sig_print_cstr = module.make_signature();
    sig_print_cstr
        .params
        .push(AbiParam::new(module.target_config().pointer_type()));
    let id_print_cstr = module
        .declare_function("qtrt_print_cstr", Linkage::Import, &sig_print_cstr)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_print_cstr".to_string(), id_print_cstr);

    // qtrt_alloc
    let mut sig_alloc = module.make_signature();
    sig_alloc.params.push(AbiParam::new(types::I64)); // size
    sig_alloc
        .returns
        .push(AbiParam::new(module.target_config().pointer_type()));
    let id_alloc = module
        .declare_function("qtrt_alloc", Linkage::Import, &sig_alloc)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_alloc".to_string(), id_alloc);

    // qtrt_free
    let mut sig_free = module.make_signature();
    sig_free
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // ptr
    sig_free.params.push(AbiParam::new(types::I64)); // size
    let id_free = module
        .declare_function("qtrt_free", Linkage::Import, &sig_free)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_free".to_string(), id_free);

    // quad_list_new
    // Returns FatPtr (3 words: ptr, len, cap)
    // For now, we fit FatPtr into return values? Cranelift might split a struct return.
    // The current Qtri compiler usually treats structs as stack-owned (via pointer) or scalar.
    // Ideally `quad_list_new` should return a Struct, and we handle the ABI.
    // However, for Stage 1 MVP, let's treat FatPtr as a struct we pass a pointer TO,
    // or we can just assume it returns by value in registers if it fits?
    // FatPtr is 3 usize (24 bytes). Common calling conventions (System V) might spill to memory or use multiple regs.
    // Let's check `qtrt` def: `pub extern "C" fn quad_list_new() -> FatPtr`.
    // The easiest way to handle struct return in Cranelift is 'sret' (Structure Return), passing a pointer as first arg.
    // WE NEED TO CHECK IF RUST DOES SRET AUTOMATICALLY FOR THIS SIZE.
    // 3 * 64bit usually uses sret.

    // qtrt_list_push
    // fn qtrt_list_push(list: *mut FatPtr, elem: *const u8, elem_size: usize)
    let mut sig_list_push = module.make_signature();
    sig_list_push
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // list ptr
    sig_list_push
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // elem ptr
    sig_list_push.params.push(AbiParam::new(types::I64)); // elem_size
    let id_list_push = module
        .declare_function("qtrt_list_push", Linkage::Import, &sig_list_push)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_list_push".to_string(), id_list_push);

    // qtrt_print_str
    // fn qtrt_print_str(s: FatPtr);
    let mut sig_print_str = module.make_signature();
    sig_print_str
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // s: *mut FatPtr
    let id_print_str = module
        .declare_function("qtrt_print_str", Linkage::Import, &sig_print_str)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_print_str".to_string(), id_print_str);

    // qtrt_print_str_no_nl
    // fn qtrt_print_str_no_nl(s: *const FatPtr);
    let mut sig_print_str_no_nl = module.make_signature();
    sig_print_str_no_nl
        .params
        .push(AbiParam::new(module.target_config().pointer_type()));
    let id_print_str_no_nl = module
        .declare_function(
            "qtrt_print_str_no_nl",
            Linkage::Import,
            &sig_print_str_no_nl,
        )
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_print_str_no_nl".to_string(), id_print_str_no_nl);

    // qtrt_panic
    // fn qtrt_panic(ptr: *const u8, len: i64) -> ! (treated as void)
    let mut sig_panic = module.make_signature();
    sig_panic
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // ptr
    sig_panic.params.push(AbiParam::new(types::I64)); // len
    let id_panic = module
        .declare_function("qtrt_panic", Linkage::Import, &sig_panic)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_panic".to_string(), id_panic);

    // qtrt_string_len
    // fn qtrt_string_len(s: FatPtr) -> i64;
    let mut sig_string_len = module.make_signature();
    sig_string_len
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // s: *mut FatPtr
    sig_string_len.returns.push(AbiParam::new(types::I64));
    let id_string_len = module
        .declare_function("qtrt_string_len", Linkage::Import, &sig_string_len)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_string_len".to_string(), id_string_len);

    // qtrt_string_concat
    // fn qtrt_string_concat(s1: FatPtr, s2: FatPtr) -> FatPtr;
    let mut sig_string_concat = module.make_signature();
    sig_string_concat
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // sret ptr
    sig_string_concat
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // s1: *mut FatPtr
    sig_string_concat
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // s2: *mut FatPtr
    let id_string_concat = module
        .declare_function("qtrt_string_concat", Linkage::Import, &sig_string_concat)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_string_concat".to_string(), id_string_concat);

    // qtrt_string_eq
    // fn qtrt_string_eq(s1: FatPtr, s2: FatPtr) -> bool;
    let mut sig_string_eq = module.make_signature();
    sig_string_eq
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // s1: *mut FatPtr
    sig_string_eq
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // s2: *mut FatPtr
    sig_string_eq.returns.push(AbiParam::new(types::I8)); // bool (u8)
    let id_string_eq = module
        .declare_function("qtrt_string_eq", Linkage::Import, &sig_string_eq)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_string_eq".to_string(), id_string_eq);

    // qtrt_read_file
    // fn qtrt_read_file(sret: *mut FatPtr, path: *const FatPtr)
    let mut sig_read_file = module.make_signature();
    sig_read_file
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // sret ptr
    sig_read_file
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // path: *const FatPtr
    let id_read_file = module
        .declare_function("qtrt_read_file", Linkage::Import, &sig_read_file)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_read_file".to_string(), id_read_file);

    // qtrt_write_file
    // fn qtrt_write_file(path: *const FatPtr, content: *const FatPtr)
    let mut sig_write_file = module.make_signature();
    sig_write_file
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // path
    sig_write_file
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // content
    let id_write_file = module
        .declare_function("qtrt_write_file", Linkage::Import, &sig_write_file)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_write_file".to_string(), id_write_file);

    // qtrt_itoa
    // fn qtrt_itoa(sret: *mut FatPtr, n: i64)
    let mut sig_itoa = module.make_signature();
    sig_itoa
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // sret
    sig_itoa.params.push(AbiParam::new(types::I64)); // n
    let id_itoa = module
        .declare_function("qtrt_itoa", Linkage::Import, &sig_itoa)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_itoa".to_string(), id_itoa);

    // qtrt_atoi
    // fn qtrt_atoi(s: *const FatPtr, out: *mut i64) -> i64 (bool)
    let mut sig_atoi = module.make_signature();
    sig_atoi
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // s: *const FatPtr
    sig_atoi
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // out: *mut i64
    sig_atoi.returns.push(AbiParam::new(types::I64));
    let id_atoi = module
        .declare_function("qtrt_atoi", Linkage::Import, &sig_atoi)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_atoi".to_string(), id_atoi);

    // qtrt_list_with_capacity
    // fn qtrt_list_with_capacity(elem_size: usize, capacity: usize) -> FatPtr
    // ABI: sret (pointer to return value) is first arg.
    let mut sig_list_cap = module.make_signature();
    sig_list_cap
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // sret ptr
    sig_list_cap.params.push(AbiParam::new(types::I64)); // elem_size
    sig_list_cap.params.push(AbiParam::new(types::I64)); // capacity
    // returns void (data written to sret ptr)
    let id_list_cap = module
        .declare_function("qtrt_list_with_capacity", Linkage::Import, &sig_list_cap)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_list_with_capacity".to_string(), id_list_cap);

    // qtrt_list_drop
    // fn qtrt_list_drop(list: *mut FatPtr, elem_size: usize)
    let mut sig_list_drop = module.make_signature();
    sig_list_drop
        .params
        .push(AbiParam::new(module.target_config().pointer_type())); // list ptr
    sig_list_drop.params.push(AbiParam::new(types::I64)); // elem_size
    let id_list_drop = module
        .declare_function("qtrt_list_drop", Linkage::Import, &sig_list_drop)
        .map_err(|e| {
            eerr(
                Span {
                    file_id: 0,
                    line: 0,
                    col: 0,
                },
                e.to_string(),
            )
        })?;
    ext_fns.insert("qtrt_list_drop".to_string(), id_list_drop);

    // 3. Declare all user functions first (to handle forward references)
    let mut user_fns: HashMap<String, cranelift_module::FuncId> = HashMap::new();
    for f in &prog.funcs {
        let sig_sem = sem
            .fns
            .get(&f.name)
            .ok_or_else(|| eerr(f.span, "missing sem sig"))?;
        let mut sig_cl = module.make_signature();

        let ptr_ty = module.target_config().pointer_type();
        let is_aggregate_ret = matches!(
            sig_sem.ret,
            Ty::Struct(_) | Ty::Enum(_) | Ty::Array { .. } | Ty::Text
        );
        if sig_sem.ret != Ty::Void && is_aggregate_ret {
            // sret pointer is the first argument
            sig_cl.params.push(AbiParam::new(ptr_ty));
        }

        for (_, ty) in &sig_sem.params {
            sig_cl.params.push(AbiParam::new(ty_cl(ty.clone(), ptr_ty)));
        }

        if sig_sem.ret != Ty::Void && !is_aggregate_ret {
            sig_cl
                .returns
                .push(AbiParam::new(ty_cl(sig_sem.ret.clone(), ptr_ty)));
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

        let ptr_ty = module.target_config().pointer_type();
        let is_aggregate_ret = matches!(
            sig_sem.ret,
            Ty::Struct(_) | Ty::Enum(_) | Ty::Array { .. } | Ty::Text
        );

        ctx.func.signature = module.make_signature();
        if sig_sem.ret != Ty::Void && is_aggregate_ret {
            ctx.func.signature.params.push(AbiParam::new(ptr_ty));
        }
        for (_, ty) in &sig_sem.params {
            ctx.func
                .signature
                .params
                .push(AbiParam::new(ty_cl(ty.clone(), ptr_ty)));
        }
        if sig_sem.ret != Ty::Void && !is_aggregate_ret {
            ctx.func
                .signature
                .returns
                .push(AbiParam::new(ty_cl(sig_sem.ret.clone(), ptr_ty)));
        }

        {
            let mut builder = FunctionBuilder::new(&mut ctx.func, &mut func_ctx);
            let entry_block = builder.create_block();
            builder.append_block_params_for_function_params(entry_block);
            builder.switch_to_block(entry_block);
            builder.seal_block(entry_block);

            let mut vars: HashMap<String, VarBinding> = HashMap::new();

            let mut param_offset = 0usize;
            let sret_ptr = if sig_sem.ret != Ty::Void && is_aggregate_ret {
                let v = builder.block_params(entry_block)[0];
                param_offset = 1;
                Some(v)
            } else {
                None
            };

            // Define params as variables
            for (i, (name, ty)) in sig_sem.params.iter().enumerate() {
                let val = builder.block_params(entry_block)[i + param_offset];
                match ty {
                    Ty::Struct(_) | Ty::Enum(_) | Ty::Array { .. } | Ty::Text => {
                        vars.insert(
                            name.clone(),
                            VarBinding::AggregateRef {
                                addr: val,
                                ty: ty.clone(),
                            },
                        );
                    }
                    _ => {
                        let var = builder.declare_var(ty_cl(ty.clone(), ptr_ty));
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

            let mut comp_ctx = CompilerCtx {
                builder: &mut builder,
                module: &mut module,
                vars: vec![vars],
                loops: vec![],
                str_data_ids: &str_data_ids,
                ext_fns: &ext_fns,
                user_fns: &user_fns,
                ptr_ty,
                sem,
                layouts: HashMap::new(),
                fn_ret: sig_sem.ret.clone(),
                sret: sret_ptr,
            };

            for stmt in &f.body {
                compile_stmt(stmt, &mut comp_ctx)?;
            }

            // Fallthrough return for void / sret functions if not present
            if (sig_sem.ret == Ty::Void || is_aggregate_ret) && !current_block_terminated(&comp_ctx)
            {
                comp_ctx.builder.ins().return_(&[]);
            } else {
                // If non-void function reaches end without return, it's UB or checked by sem.
                // We can insert a trap or dummy return if needed, but sem checks should prevent this.
            }

            builder.finalize();
        }

        module
            .define_function(*fid, &mut ctx)
            .map_err(|e| eerr(f.span, format!("{e:?}\n\n{}", ctx.func.display())))?;
        module.clear_context(&mut ctx);
    }

    let product = module.finish();
    Ok(product.emit().map_err(|e| {
        eerr(
            Span {
                file_id: 0,
                line: 0,
                col: 0,
            },
            e.to_string(),
        )
    })?)
}

struct CompilerCtx<'a, 'b> {
    builder: &'a mut FunctionBuilder<'b>,
    module: &'a mut ObjectModule,
    vars: Vec<HashMap<String, VarBinding>>,
    loops: Vec<(Block, Block)>, // (break_block, continue_block)
    str_data_ids: &'a HashMap<String, cranelift_module::DataId>,
    ext_fns: &'a HashMap<String, cranelift_module::FuncId>,
    user_fns: &'a HashMap<String, cranelift_module::FuncId>,
    ptr_ty: Type,
    sem: &'a SemInfo,
    layouts: HashMap<String, StructLayout>,
    fn_ret: Ty,
    sret: Option<Value>,
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
        let var = self.builder.declare_var(ty);
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
    AggregateOwned { slot: StackSlot, ty: Ty },
    AggregateRef { addr: Value, ty: Ty },
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
    AggregatePtr {
        addr: Value,
        ty: Ty,
        slot: Option<StackSlot>,
    },
}

fn current_block_terminated(ctx: &CompilerCtx) -> bool {
    let Some(b) = ctx.builder.current_block() else {
        return true;
    };
    let Some(inst) = ctx.builder.func.layout.last_inst(b) else {
        return false;
    };
    matches!(
        ctx.builder.func.dfg.insts[inst].opcode(),
        Opcode::Return | Opcode::Jump | Opcode::Brif | Opcode::BrTable | Opcode::Trap
    )
}

fn compile_stmt(stmt: &Stmt, ctx: &mut CompilerCtx) -> Result<(), EmitError> {
    match stmt {
        Stmt::VarDecl { name, ty, init, .. } => {
            let init_val = compile_expr(init, ctx)?;
            let sem_ty = crate::sem::parse_ty(ty, &ctx.sem.structs, &ctx.sem.enums).unwrap();
            match sem_ty {
                Ty::Struct(_) | Ty::Enum(_) | Ty::Array { .. } | Ty::Text => {
                    let (size, align) = ty_size_align(&sem_ty, ctx, init.span())?;
                    let align_shift = align.trailing_zeros() as u8;

                    let slot = match init_val {
                        ValueKind::AggregatePtr { slot: Some(s), .. } => s,
                        ValueKind::AggregatePtr { addr, ty, .. } => {
                            // verify type matches?
                            if ty != sem_ty {
                                return Err(eerr(init.span(), "type mismatch in var decl"));
                            }
                            let new_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                size as u32,
                                align_shift,
                            ));
                            let dst = ctx.builder.ins().stack_addr(ctx.ptr_ty, new_slot, 0);
                            copy_memory(addr, dst, size, ctx);
                            new_slot
                        }
                        ValueKind::Scalar(_, _) => {
                            return Err(eerr(init.span(), "expected aggregate value"));
                        }
                    };
                    ctx.vars.last_mut().unwrap().insert(
                        name.clone(),
                        VarBinding::AggregateOwned { slot, ty: sem_ty },
                    );
                }
                _ => {
                    let (val, vty) = match init_val {
                        ValueKind::Scalar(v, ty) => (v, ty),
                        ValueKind::AggregatePtr { .. } => {
                            return Err(eerr(init.span(), "expected scalar value"));
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
                                ValueKind::AggregatePtr { .. } => {
                                    return Err(eerr(*span, "type mismatch in assignment"));
                                }
                            };
                            if vty != ty {
                                return Err(eerr(*span, "type mismatch in assignment"));
                            }
                            ctx.builder.def_var(var, v);
                        }
                        VarBinding::AggregateOwned { slot, ty: sty } => {
                            let src_addr = match val {
                                ValueKind::AggregatePtr { addr, ty, .. } => {
                                    if ty != sty {
                                        return Err(eerr(*span, "aggregate type mismatch"));
                                    }
                                    addr
                                }
                                _ => return Err(eerr(*span, "type mismatch in assignment")),
                            };
                            let (size, _) = ty_size_align(&sty, ctx, *span)?;
                            let dst_addr = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);
                            copy_memory(src_addr, dst_addr, size, ctx);
                        }
                        VarBinding::AggregateRef { addr, ty: sty } => {
                            let src_addr = match val {
                                ValueKind::AggregatePtr { addr, ty, .. } => {
                                    if ty != sty {
                                        return Err(eerr(*span, "aggregate type mismatch"));
                                    }
                                    addr
                                }
                                _ => return Err(eerr(*span, "type mismatch in assignment")),
                            };
                            let (size, _) = ty_size_align(&sty, ctx, *span)?;
                            copy_memory(src_addr, addr, size, ctx);
                        }
                    }
                }
                AssignTarget::Field { base, field } => {
                    let binding = ctx
                        .get_var(base)
                        .ok_or_else(|| eerr(*span, "unknown var"))?;
                    let (sname, addr) = match binding {
                        VarBinding::AggregateOwned { slot, ty } => match ty {
                            Ty::Struct(n) => (n, ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0)),
                            _ => return Err(eerr(*span, "field assign on non-struct")),
                        },
                        VarBinding::AggregateRef { addr, ty } => match ty {
                            Ty::Struct(n) => (n, addr),
                            _ => return Err(eerr(*span, "field assign on non-struct")),
                        },
                        VarBinding::Scalar { var, ty } => match ty {
                            Ty::Ref(inner) => match *inner {
                                Ty::Struct(n) => (n, ctx.builder.use_var(var)),
                                _ => return Err(eerr(*span, "field assign on non-struct")),
                            },
                            _ => return Err(eerr(*span, "field assign on non-struct")),
                        },
                    };
                    let layout = get_struct_layout(&sname, ctx, *span)?;
                    let fld = layout
                        .fields
                        .iter()
                        .find(|f| f.name.as_str() == field.as_str())
                        .ok_or_else(|| eerr(*span, "unknown field"))?;
                    let (v, vty) = match val {
                        ValueKind::Scalar(v, ty) => (v, ty),
                        ValueKind::AggregatePtr { .. } => {
                            return Err(eerr(*span, "type mismatch in field assignment"));
                        }
                    };
                    if vty != fld.ty {
                        return Err(eerr(*span, "type mismatch in field assignment"));
                    }
                    store_field(addr, fld.offset, v, &fld.ty, ctx)?;
                }
                AssignTarget::Index { base, index } => {
                    let binding = ctx
                        .get_var(base)
                        .ok_or_else(|| eerr(*span, "unknown var"))?;
                    let (elem_ty, addr) = match binding {
                        VarBinding::AggregateOwned {
                            slot,
                            ty: Ty::Array { elem },
                        } => (
                            *elem.clone(),
                            ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0),
                        ),
                        VarBinding::AggregateRef {
                            addr,
                            ty: Ty::Array { elem },
                        } => (*elem.clone(), addr),
                        _ => return Err(eerr(*span, "index assign on non-array")),
                    };
                    let (elem_size, _) = ty_size_align(&elem_ty, ctx, *span)?;

                    let (idx_val, _) = expect_scalar(compile_expr(index, ctx)?, *span)?;
                    // Dynamic array is ptr, len, cap. "addr" is &FatPtr.
                    // We need to load .ptr (offset 0).
                    // This is a "double indirection" if we treat FatPtr as aggregate on stack.
                    // Actually `VarBinding` stores the stack address of the FatPtr struct for Owned.
                    // So `addr` = &FatPtr.
                    // We need `data_ptr = *addr`.
                    let data_ptr = ctx.builder.ins().load(ctx.ptr_ty, MemFlags::new(), addr, 0);

                    let offset = ctx.builder.ins().imul_imm(idx_val, elem_size as i64);
                    let elem_addr = ctx.builder.ins().iadd(data_ptr, offset);

                    match val {
                        ValueKind::Scalar(v, vty) => {
                            if vty != elem_ty {
                                return Err(eerr(*span, "type mismatch in assignment"));
                            }
                            // assuming scalar store
                            ctx.builder.ins().store(MemFlags::new(), v, elem_addr, 0);
                        }
                        ValueKind::AggregatePtr { addr: src, ty, .. } => {
                            if ty != elem_ty {
                                return Err(eerr(*span, "type mismatch in assignment"));
                            }
                            copy_memory(src, elem_addr, elem_size, ctx);
                        }
                    }
                }
            }
        }
        Stmt::Expr { expr, .. } => {
            let _ = compile_expr(expr, ctx)?;
        }
        Stmt::Back { expr, span } => {
            let is_aggregate_ret = matches!(
                ctx.fn_ret,
                Ty::Struct(_) | Ty::Enum(_) | Ty::Array { .. } | Ty::Text
            );

            if is_aggregate_ret {
                let sret = ctx
                    .sret
                    .ok_or_else(|| eerr(*span, "missing sret pointer for aggregate return"))?;
                let ret_ty = ctx.fn_ret.clone();
                let (ret_sz, _) = ty_size_align(&ret_ty, ctx, *span)?;

                let Some(e) = expr else {
                    return Err(eerr(*span, "missing return value"));
                };
                match compile_expr(e, ctx)? {
                    ValueKind::AggregatePtr { addr, ty, .. } => {
                        if ty != ret_ty {
                            return Err(eerr(*span, "return type mismatch"));
                        }
                        copy_memory(addr, sret, ret_sz, ctx);
                        ctx.builder.ins().return_(&[]);
                    }
                    ValueKind::Scalar(_, _) => {
                        return Err(eerr(*span, "expected aggregate return value"));
                    }
                }
            } else if let Some(e) = expr {
                let val = match compile_expr(e, ctx)? {
                    ValueKind::Scalar(v, _) => v,
                    ValueKind::AggregatePtr { .. } => {
                        return Err(eerr(*span, "unexpected aggregate in scalar return"));
                    }
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

                if !current_block_terminated(ctx) {
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

            if !current_block_terminated(ctx) {
                ctx.builder.ins().jump(exit_block, &[]);
            }

            ctx.builder.switch_to_block(exit_block);
            ctx.builder.seal_block(exit_block);
        }
        Stmt::Case {
            scrutinee,
            arms,
            span,
        } => {
            let val = compile_expr(scrutinee, ctx)?;
            let (addr, enum_name) = match val {
                ValueKind::AggregatePtr {
                    addr,
                    ty: Ty::Enum(name),
                    ..
                } => (addr, name),
                _ => return Err(eerr(*span, "case on non-enum")),
            };

            // Load Tag (offset 0)
            let tag_val = ctx.builder.ins().load(types::I64, MemFlags::new(), addr, 0);

            let einfo = ctx
                .sem
                .enums
                .get(&enum_name)
                .ok_or_else(|| eerr(*span, "unknown enum"))?
                .clone();

            // Sort variants for canonical tagging
            let mut sorted_variants: Vec<_> = einfo.variants.iter().collect();
            sorted_variants.sort_by_key(|v| v.0);

            // Calculate payload offset (same as ty_size_align logic)
            let mut max_payload_align = 1;
            for (_, fields) in &sorted_variants {
                let mut v_align = 1;
                for fty in *fields {
                    let (_, al) = ty_size_align(fty, ctx, *span)?;
                    v_align = v_align.max(al);
                }
                max_payload_align = max_payload_align.max(v_align);
            }
            let payload_offset = align_up(8, max_payload_align);

            let merge_block = ctx.builder.create_block();

            // Emit dispatch logic
            let mut arm_blocks = Vec::new();
            for _ in 0..arms.len() {
                arm_blocks.push(ctx.builder.create_block());
            }

            for (i, (pat, _)) in arms.iter().enumerate() {
                let (vname, _) = match pat {
                    Pattern::EnumVariant { variant, .. } => (variant, ()),
                };
                let tag_idx = sorted_variants
                    .iter()
                    .position(|(k, _)| *k == vname)
                    .ok_or_else(|| eerr(*span, "unknown variant in pattern"))?;

                let next_test_block = ctx.builder.create_block();

                let idx_val = ctx.builder.ins().iconst(types::I64, tag_idx as i64);
                let is_match = ctx.builder.ins().icmp(IntCC::Equal, tag_val, idx_val);

                // brif(cond, then_block, then_args, else_block, else_args)
                ctx.builder
                    .ins()
                    .brif(is_match, arm_blocks[i], &[], next_test_block, &[]);

                ctx.builder.switch_to_block(next_test_block);
                ctx.builder.seal_block(next_test_block);
            }

            // Fallthrough to merge/default
            ctx.builder.ins().jump(merge_block, &[]);

            // ... (rest of Case)
            // Skipping to EnumLit fix for borrow checker
            // We need to target specific lines for that.
            // Let's do two replacements.

            // Compile Arms
            for (i, (pat, body)) in arms.iter().enumerate() {
                let blk = arm_blocks[i];
                ctx.builder.switch_to_block(blk);
                ctx.builder.seal_block(blk);

                ctx.vars.push(HashMap::new()); // New scope for bindings

                // Bindings
                match pat {
                    Pattern::EnumVariant { bindings, .. } => {
                        let (vname, _) = match pat {
                            Pattern::EnumVariant { variant, .. } => (variant, ()),
                        };
                        let fields = sorted_variants
                            .iter()
                            .find(|(k, _)| *k == vname)
                            .ok_or_else(|| eerr(*span, "variant missing in sorted list"))?
                            .1;

                        let mut current_offset = payload_offset;
                        for (j, bind_name) in bindings.iter().enumerate() {
                            let fty = &(*fields)[j];
                            let (sz, al) = ty_size_align(fty, ctx, *span)?;
                            current_offset = align_up(current_offset, al);

                            let val = load_field(addr, current_offset, fty, ctx)?;

                            let cl_ty = ty_cl(fty.clone(), ctx.ptr_ty);
                            let var = ctx.declare_scalar(bind_name.clone(), cl_ty, fty.clone());

                            ctx.builder.def_var(var, val);

                            current_offset += sz;
                        }
                    }
                }

                for s in body {
                    compile_stmt(s, ctx)?;
                }

                ctx.vars.pop(); // Pop binding scope

                if !current_block_terminated(ctx) {
                    ctx.builder.ins().jump(merge_block, &[]);
                }
            }

            ctx.builder.switch_to_block(merge_block);
            ctx.builder.seal_block(merge_block);
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

            if !current_block_terminated(ctx) {
                ctx.builder.ins().jump(header_block, &[]);
            }

            ctx.builder.switch_to_block(exit_block);
            ctx.builder.seal_block(header_block);
            ctx.builder.seal_block(body_block);
            ctx.builder.seal_block(exit_block);
        }
        Stmt::Foreach {
            name,
            ty,
            iter,
            body,
            span,
        } => {
            let loop_var_ty = crate::sem::parse_ty(ty, &ctx.sem.structs, &ctx.sem.enums)
                .ok_or_else(|| eerr(*span, "unknown foreach variable type"))?;

            // Evaluate iterator once and keep a stable receiver pointer.
            let iter_val = compile_expr(iter, ctx)?;
            let (iter_struct, recv_ptr) = match iter_val {
                ValueKind::AggregatePtr {
                    addr,
                    ty: Ty::Struct(sname),
                    slot,
                } => {
                    // Ensure iterator lives in a stack slot we can mutate via methods.
                    let recv = if let Some(s) = slot {
                        ctx.builder.ins().stack_addr(ctx.ptr_ty, s, 0)
                    } else {
                        let (sz, al) = ty_size_align(&Ty::Struct(sname.clone()), ctx, *span)?;
                        let ss = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            sz as u32,
                            al.trailing_zeros() as u8,
                        ));
                        let dst = ctx.builder.ins().stack_addr(ctx.ptr_ty, ss, 0);
                        copy_memory(addr, dst, sz, ctx);
                        dst
                    };
                    (sname, recv)
                }
                ValueKind::Scalar(ptr, Ty::Ref(inner)) => match *inner {
                    Ty::Struct(sname) => (sname, ptr),
                    _ => return Err(eerr(*span, "foreach iterator must be a struct")),
                },
                _ => return Err(eerr(*span, "foreach iterator must be a struct")),
            };

            let msig = ctx
                .sem
                .methods
                .get(&iter_struct)
                .and_then(|m| m.get("next"))
                .ok_or_else(|| eerr(*span, "foreach iterator missing next() method"))?;

            let opt_enum = match &msig.ret {
                Ty::Enum(n) => n.clone(),
                _ => return Err(eerr(*span, "next() must return Option<T>")),
            };
            let einfo = ctx
                .sem
                .enums
                .get(&opt_enum)
                .ok_or_else(|| eerr(*span, "unknown Option<T> enum"))?
                .clone();

            // Allocate a stack slot for the Option result.
            let (opt_sz, opt_al) = ty_size_align(&Ty::Enum(opt_enum.clone()), ctx, *span)?;
            let opt_slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                opt_sz as u32,
                opt_al.trailing_zeros() as u8,
            ));
            let opt_addr = ctx.builder.ins().stack_addr(ctx.ptr_ty, opt_slot, 0);

            // Determine tags/offsets for Option.
            let some_tag = enum_tag_index(&einfo, "Some", *span)?;
            let some_off = enum_variant_field_offset(&einfo, "Some", 0, ctx, *span)?;

            let header_block = ctx.builder.create_block();
            let body_block = ctx.builder.create_block();
            let exit_block = ctx.builder.create_block();

            // Pass extracted payload to the body as a block parameter.
            let payload_param_ty = if is_aggregate_ty(&loop_var_ty) {
                ctx.ptr_ty
            } else {
                ty_cl(loop_var_ty.clone(), ctx.ptr_ty)
            };
            ctx.builder.append_block_param(body_block, payload_param_ty);

            ctx.builder.ins().jump(header_block, &[]);
            ctx.builder.switch_to_block(header_block);

            // Call next(): Option<T>
            let fid = ctx
                .user_fns
                .get(&msig.symbol)
                .ok_or_else(|| eerr(*span, "missing next() symbol"))?;
            let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
            ctx.builder.ins().call(func_ref, &[opt_addr, recv_ptr]);

            // Branch on Option tag.
            let tag = ctx
                .builder
                .ins()
                .load(types::I64, MemFlags::new(), opt_addr, 0);
            let some_tag_val = ctx.builder.ins().iconst(types::I64, some_tag);
            let is_some = ctx.builder.ins().icmp(IntCC::Equal, tag, some_tag_val);

            if is_aggregate_ty(&loop_var_ty) {
                let payload_ptr = ctx.builder.ins().iadd_imm(opt_addr, some_off as i64);
                ctx.builder.ins().brif(
                    is_some,
                    body_block,
                    &[BlockArg::from(payload_ptr)],
                    exit_block,
                    &[],
                );
            } else {
                let cl_ty = ty_cl(loop_var_ty.clone(), ctx.ptr_ty);
                let payload_val =
                    ctx.builder
                        .ins()
                        .load(cl_ty, MemFlags::new(), opt_addr, some_off);
                ctx.builder.ins().brif(
                    is_some,
                    body_block,
                    &[BlockArg::from(payload_val)],
                    exit_block,
                    &[],
                );
            }

            ctx.builder.switch_to_block(body_block);
            // Loop body
            ctx.loops.push((exit_block, header_block));
            ctx.vars.push(HashMap::new());

            let payload = ctx.builder.block_params(body_block)[0];
            match loop_var_ty.clone() {
                Ty::Struct(_) | Ty::Enum(_) | Ty::Array { .. } | Ty::Text => {
                    let (sz, al) = ty_size_align(&loop_var_ty, ctx, *span)?;
                    let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        sz as u32,
                        al.trailing_zeros() as u8,
                    ));
                    let dst = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);
                    copy_memory(payload, dst, sz, ctx);
                    ctx.vars.last_mut().unwrap().insert(
                        name.clone(),
                        VarBinding::AggregateOwned {
                            slot,
                            ty: loop_var_ty.clone(),
                        },
                    );
                }
                _ => {
                    let cl_ty = ty_cl(loop_var_ty.clone(), ctx.ptr_ty);
                    let var = ctx.declare_scalar(name.clone(), cl_ty, loop_var_ty.clone());
                    ctx.builder.def_var(var, payload);
                }
            }

            for s in body {
                compile_stmt(s, ctx)?;
            }

            ctx.vars.pop();
            ctx.loops.pop();

            if !current_block_terminated(ctx) {
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
            let (_, nxt) = ctx
                .loops
                .last()
                .ok_or_else(|| eerr(*span, "continue outside loop"))?;
            ctx.builder.ins().jump(*nxt, &[]);
        }
    }
    Ok(())
}

fn compile_expr(expr: &Expr, ctx: &mut CompilerCtx) -> Result<ValueKind, EmitError> {
    match expr {
        Expr::Bool(b, _) => Ok(ValueKind::Scalar(
            ctx.builder.ins().iconst(types::I8, if *b { 1 } else { 0 }),
            Ty::Bool,
        )),
        Expr::Int(n, _) => Ok(ValueKind::Scalar(
            ctx.builder.ins().iconst(types::I64, *n),
            Ty::Int,
        )),
        Expr::Str(s, _) => {
            let data_id = ctx.str_data_ids.get(s).unwrap();
            let global_val = ctx.module.declare_data_in_func(*data_id, ctx.builder.func);
            let ptr = ctx.builder.ins().global_value(ctx.ptr_ty, global_val);

            // Allocate stack slot for FatPtr (24 bytes)
            let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                24,
                3, // align 8
            ));
            let addr = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

            // Store ptr (offset 0)
            ctx.builder.ins().store(MemFlags::new(), ptr, addr, 0);

            // Store len (offset 8)
            let len_val = ctx.builder.ins().iconst(types::I64, s.len() as i64);
            ctx.builder.ins().store(MemFlags::new(), len_val, addr, 8);

            // Store cap (offset 16) - 0 for static string
            let cap_val = ctx.builder.ins().iconst(types::I64, 0);
            ctx.builder.ins().store(MemFlags::new(), cap_val, addr, 16);

            Ok(ValueKind::AggregatePtr {
                addr,
                ty: Ty::Text,
                slot: Some(slot),
            })
        }
        Expr::ArrayLit { elements, span } => {
            if elements.is_empty() {
                return Err(eerr(*span, "empty array literal"));
            }

            // 1. Compile first element to deduce type
            let first_val = compile_expr(&elements[0], ctx)?;
            let (first_v, elem_ty) = match first_val {
                ValueKind::Scalar(v, t) => (Some(v), t),
                ValueKind::AggregatePtr { addr, ty, .. } => (Some(addr), ty),
            };

            let (elem_size, _) = ty_size_align(&elem_ty, ctx, *span)?;
            let len = elements.len() as i32;

            // 2. Allocate stack slot for FatPtr (24 bytes)
            let fatptr_size = 24;
            let fatptr_align_shift = 3; // 8 bytes alignment
            let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                fatptr_size,
                fatptr_align_shift,
            ));
            let list_ptr = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

            // 3. Call qtrt_list_with_capacity(sret=list_ptr, elem_size, capacity=len)
            let fid = ctx.ext_fns.get("qtrt_list_with_capacity").unwrap();
            let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
            let elem_size_val = ctx.builder.ins().iconst(types::I64, elem_size as i64);
            let cap_val = ctx.builder.ins().iconst(types::I64, len as i64);

            ctx.builder
                .ins()
                .call(func_ref, &[list_ptr, elem_size_val, cap_val]);

            // Helper to push value
            // We need a loop over elements.
            // Problem: closure borrowing ctx mutably while we iterate?
            // We can just inline logic.

            // Push first
            {
                let v = first_v.unwrap();
                let addr = match &elem_ty {
                    Ty::Int | Ty::Bool | Ty::Text => {
                        let (sz, al) = ty_size_align(&elem_ty, ctx, *span)?;
                        let ss = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            sz as u32,
                            al.trailing_zeros() as u8,
                        ));
                        let sp = ctx.builder.ins().stack_addr(ctx.ptr_ty, ss, 0);
                        store_elem(sp, 0, v, &elem_ty, sz, ctx)?;
                        sp
                    }
                    _ => v, // Aggregate ptr is already an address
                };

                let fid_push = ctx.ext_fns.get("qtrt_list_push").unwrap();
                let fref_push = ctx.module.declare_func_in_func(*fid_push, ctx.builder.func);
                let sz_val = ctx.builder.ins().iconst(types::I64, elem_size as i64);
                ctx.builder.ins().call(fref_push, &[list_ptr, addr, sz_val]);
            }

            // Push rest
            for e in elements.iter().skip(1) {
                let val = compile_expr(e, ctx)?;
                let (v, ty) = match val {
                    ValueKind::Scalar(v, t) => (v, t),
                    ValueKind::AggregatePtr { addr, ty, .. } => (addr, ty),
                };

                if ty != elem_ty {
                    return Err(eerr(e.span(), "mismatch"));
                }

                let addr = match &elem_ty {
                    Ty::Int | Ty::Bool | Ty::Text => {
                        let (sz, al) = ty_size_align(&elem_ty, ctx, *span)?;
                        let ss = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            sz as u32,
                            al.trailing_zeros() as u8,
                        ));
                        let sp = ctx.builder.ins().stack_addr(ctx.ptr_ty, ss, 0);
                        store_elem(sp, 0, v, &elem_ty, sz, ctx)?;
                        sp
                    }
                    _ => v,
                };

                let fid_push = ctx.ext_fns.get("qtrt_list_push").unwrap();
                let fref_push = ctx.module.declare_func_in_func(*fid_push, ctx.builder.func);
                let sz_val = ctx.builder.ins().iconst(types::I64, elem_size as i64);
                ctx.builder.ins().call(fref_push, &[list_ptr, addr, sz_val]);
            }

            Ok(ValueKind::AggregatePtr {
                addr: list_ptr,
                ty: Ty::Array {
                    elem: Box::new(elem_ty),
                },
                slot: Some(slot),
            })
        }
        Expr::Ident(name, span) => match ctx.get_var(name) {
            Some(VarBinding::Scalar { var, ty }) => {
                Ok(ValueKind::Scalar(ctx.builder.use_var(var), ty))
            }
            Some(VarBinding::AggregateOwned { slot, ty }) => Ok(ValueKind::AggregatePtr {
                addr: ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0),
                ty,
                slot: Some(slot),
            }),
            Some(VarBinding::AggregateRef { addr, ty }) => Ok(ValueKind::AggregatePtr {
                addr,
                ty,
                slot: None,
            }),
            None => Err(eerr(*span, "unknown var")),
        },
        Expr::EnumLit {
            enum_name,
            variant,
            args,
            span,
        } => {
            let einfo = ctx
                .sem
                .enums
                .get(enum_name)
                .ok_or_else(|| eerr(*span, "unknown enum"))?
                .clone();
            let mut sorted_variants: Vec<_> = einfo.variants.iter().collect();
            sorted_variants.sort_by_key(|v| v.0);

            let tag_idx = sorted_variants
                .iter()
                .position(|(k, _)| *k == variant)
                .ok_or_else(|| eerr(*span, "unknown variant"))?;

            let (size, align) = ty_size_align(&Ty::Enum(enum_name.clone()), ctx, *span)?;
            let align_shift = (align as u32).trailing_zeros() as u8;
            let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                StackSlotKind::ExplicitSlot,
                size as u32,
                align_shift,
            ));
            let addr = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

            let tag_const = ctx.builder.ins().iconst(types::I64, tag_idx as i64);
            ctx.builder.ins().store(MemFlags::new(), tag_const, addr, 0);

            let mut max_payload_align = 1;
            for (_, fields) in &sorted_variants {
                let mut v_align = 1;
                for fty in *fields {
                    let (_, al) = ty_size_align(fty, ctx, *span)?;
                    v_align = v_align.max(al);
                }
                max_payload_align = max_payload_align.max(v_align);
            }
            let payload_offset = align_up(8, max_payload_align);

            let variant_fields = sorted_variants[tag_idx].1;
            let mut current_offset = payload_offset;

            for (i, expr) in args.iter().enumerate() {
                let fty = &variant_fields[i];
                let (sz, al) = ty_size_align(fty, ctx, *span)?;
                current_offset = align_up(current_offset, al);

                let val = compile_expr(expr, ctx)?;
                match val {
                    ValueKind::Scalar(v, _) => {
                        ctx.builder
                            .ins()
                            .store(MemFlags::new(), v, addr, current_offset as i32);
                    }
                    ValueKind::AggregatePtr { addr: src, ty, .. } => {
                        let dst = ctx.builder.ins().iadd_imm(addr, current_offset as i64);
                        let (sz, _) = ty_size_align(&ty, ctx, *span)?;
                        copy_memory(src, dst, sz, ctx);
                    }
                }
                current_offset += sz;
            }

            Ok(ValueKind::AggregatePtr {
                addr,
                ty: Ty::Enum(enum_name.clone()),
                slot: Some(slot),
            })
        }
        Expr::Field { base, field, span } => match compile_expr(base, ctx)? {
            ValueKind::AggregatePtr {
                addr,
                ty: Ty::Struct(ty_name),
                ..
            } => {
                let layout = get_struct_layout(&ty_name, ctx, *span)?;
                let fld = layout
                    .fields
                    .iter()
                    .find(|f| f.name.as_str() == field.as_str())
                    .ok_or_else(|| eerr(*span, "unknown field"))?;
                if is_aggregate_ty(&fld.ty) {
                    let field_addr = ctx.builder.ins().iadd_imm(addr, fld.offset as i64);
                    Ok(ValueKind::AggregatePtr {
                        addr: field_addr,
                        ty: fld.ty.clone(),
                        slot: None,
                    })
                } else {
                    let val = load_field(addr, fld.offset, &fld.ty, ctx)?;
                    Ok(ValueKind::Scalar(val, fld.ty.clone()))
                }
            }
            _ => Err(eerr(*span, "field access on non-struct")),
        },
        Expr::Index { base, index, span } => {
            let base_val = compile_expr(base, ctx)?;
            let (addr, elem_ty) = match base_val {
                ValueKind::AggregatePtr {
                    addr,
                    ty: Ty::Array { elem },
                    ..
                } => (addr, *elem),
                _ => return Err(eerr(*span, "index on non-array")),
            };
            let (idx_val, _) = expect_scalar(compile_expr(index, ctx)?, *span)?;
            let (elem_size, _) = ty_size_align(&elem_ty, ctx, *span)?;

            let offset = ctx.builder.ins().imul_imm(idx_val, elem_size as i64);

            // `addr` is &FatPtr. Load data ptr at offset 0.
            let data_ptr = ctx.builder.ins().load(ctx.ptr_ty, MemFlags::new(), addr, 0);
            let elem_addr = ctx.builder.ins().iadd(data_ptr, offset);

            // if elem_ty is scalar, load it. else return pointer.
            // Ty::Ref handling removed
            match elem_ty {
                Ty::Int | Ty::Bool => {
                    let cl_ty = ty_cl(elem_ty.clone(), ctx.ptr_ty);
                    let v = ctx.builder.ins().load(cl_ty, MemFlags::new(), elem_addr, 0);
                    Ok(ValueKind::Scalar(v, elem_ty.clone()))
                }
                _ => Ok(ValueKind::AggregatePtr {
                    addr: elem_addr,
                    ty: elem_ty,
                    slot: None,
                }),
            }
        }
        Expr::Try { expr, span } => {
            let src = compile_expr(expr, ctx)?;
            let ValueKind::AggregatePtr {
                addr: src_addr,
                ty: Ty::Enum(src_enum),
                ..
            } = src
            else {
                return Err(eerr(*span, "'?' expects Option/Result"));
            };

            let is_option = src_enum.starts_with("Option<");
            let is_result = src_enum.starts_with("Result<");
            if !is_option && !is_result {
                return Err(eerr(*span, "'?' expects Option/Result"));
            }

            let fn_enum = match &ctx.fn_ret {
                Ty::Enum(n) => n.clone(),
                _ => return Err(eerr(*span, "'?' used in non-Option/Result function")),
            };
            if is_option && !fn_enum.starts_with("Option<") {
                return Err(eerr(*span, "Option<T>? requires function returning Option"));
            }
            if is_result && !fn_enum.starts_with("Result<") {
                return Err(eerr(
                    *span,
                    "Result<T,E>? requires function returning Result",
                ));
            }

            let sret = ctx
                .sret
                .ok_or_else(|| eerr(*span, "missing sret for '?'"))?;

            let src_einfo = ctx
                .sem
                .enums
                .get(&src_enum)
                .ok_or_else(|| eerr(*span, "unknown enum"))?
                .clone();

            let ok_variant = if is_option { "Some" } else { "Ok" };
            let fail_variant = if is_option { "None" } else { "Err" };

            let ok_fields = src_einfo
                .variants
                .get(ok_variant)
                .ok_or_else(|| eerr(*span, "malformed intrinsic enum"))?;
            if ok_fields.len() != 1 {
                return Err(eerr(*span, "malformed intrinsic enum"));
            }
            let ok_ty = ok_fields[0].clone();

            let src_ok_tag = enum_tag_index(&src_einfo, ok_variant, *span)?;

            let ok_block = ctx.builder.create_block();
            let fail_block = ctx.builder.create_block();
            let cont_block = ctx.builder.create_block();

            let cont_param_ty = if is_aggregate_ty(&ok_ty) {
                ctx.ptr_ty
            } else {
                ty_cl(ok_ty.clone(), ctx.ptr_ty)
            };
            ctx.builder.append_block_param(cont_block, cont_param_ty);

            let tag = ctx
                .builder
                .ins()
                .load(types::I64, MemFlags::new(), src_addr, 0);
            let ok_tag_val = ctx.builder.ins().iconst(types::I64, src_ok_tag);
            let is_ok = ctx.builder.ins().icmp(IntCC::Equal, tag, ok_tag_val);
            ctx.builder
                .ins()
                .brif(is_ok, ok_block, &[], fail_block, &[]);

            // ok path: extract payload
            ctx.builder.switch_to_block(ok_block);
            ctx.builder.seal_block(ok_block);

            let ok_off = enum_variant_field_offset(&src_einfo, ok_variant, 0, ctx, *span)?;
            if is_aggregate_ty(&ok_ty) {
                let ptr = ctx.builder.ins().iadd_imm(src_addr, ok_off as i64);
                ctx.builder.ins().jump(cont_block, &[BlockArg::from(ptr)]);
            } else {
                let cl_ty = ty_cl(ok_ty.clone(), ctx.ptr_ty);
                let v = ctx
                    .builder
                    .ins()
                    .load(cl_ty, MemFlags::new(), src_addr, ok_off);
                ctx.builder.ins().jump(cont_block, &[BlockArg::from(v)]);
            }

            // fail path: early return
            ctx.builder.switch_to_block(fail_block);
            ctx.builder.seal_block(fail_block);

            // Store failure variant into function sret and return.
            let fn_einfo = ctx
                .sem
                .enums
                .get(&fn_enum)
                .ok_or_else(|| eerr(*span, "unknown function enum"))?
                .clone();
            let fail_tag = enum_tag_index(&fn_einfo, fail_variant, *span)?;
            let fail_tag_val = ctx.builder.ins().iconst(types::I64, fail_tag);
            ctx.builder
                .ins()
                .store(MemFlags::new(), fail_tag_val, sret, 0);

            if is_result {
                let src_err_fields = src_einfo
                    .variants
                    .get("Err")
                    .ok_or_else(|| eerr(*span, "malformed Result"))?;
                let fn_err_fields = fn_einfo
                    .variants
                    .get("Err")
                    .ok_or_else(|| eerr(*span, "malformed Result"))?;
                if src_err_fields.len() != 1 || fn_err_fields.len() != 1 {
                    return Err(eerr(*span, "malformed Result"));
                }
                let err_ty = fn_err_fields[0].clone();

                let src_err_off = enum_variant_field_offset(&src_einfo, "Err", 0, ctx, *span)?;
                let dst_err_off = enum_variant_field_offset(&fn_einfo, "Err", 0, ctx, *span)?;

                if is_aggregate_ty(&err_ty) {
                    let (esz, _) = ty_size_align(&err_ty, ctx, *span)?;
                    let src_ptr = ctx.builder.ins().iadd_imm(src_addr, src_err_off as i64);
                    let dst_ptr = ctx.builder.ins().iadd_imm(sret, dst_err_off as i64);
                    copy_memory(src_ptr, dst_ptr, esz, ctx);
                } else {
                    let cl_ty = ty_cl(err_ty.clone(), ctx.ptr_ty);
                    let v = ctx
                        .builder
                        .ins()
                        .load(cl_ty, MemFlags::new(), src_addr, src_err_off);
                    ctx.builder
                        .ins()
                        .store(MemFlags::new(), v, sret, dst_err_off);
                }
            }

            ctx.builder.ins().return_(&[]);

            // continuation
            ctx.builder.switch_to_block(cont_block);
            ctx.builder.seal_block(cont_block);
            let v = ctx.builder.block_params(cont_block)[0];
            if is_aggregate_ty(&ok_ty) {
                Ok(ValueKind::AggregatePtr {
                    addr: v,
                    ty: ok_ty,
                    slot: None,
                })
            } else {
                Ok(ValueKind::Scalar(v, ok_ty))
            }
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
            let l_val = compile_expr(lhs, ctx)?;
            let r_val = compile_expr(rhs, ctx)?;

            // Helper to get type and validation
            let lty = match &l_val {
                ValueKind::Scalar(_, t) => t.clone(),
                ValueKind::AggregatePtr { ty, .. } => ty.clone(),
            };
            let rty = match &r_val {
                ValueKind::Scalar(_, t) => t.clone(),
                ValueKind::AggregatePtr { ty, .. } => ty.clone(),
            };

            if lty == Ty::Text && rty == Ty::Text {
                let l_addr = match l_val {
                    ValueKind::AggregatePtr { addr, .. } => addr,
                    _ => unreachable!(),
                };
                let r_addr = match r_val {
                    ValueKind::AggregatePtr { addr, .. } => addr,
                    _ => unreachable!(),
                };

                match op {
                    BinOp::Add => {
                        // qtrt_string_concat(sret, l, r)
                        let (sz, al) = ty_size_align(&Ty::Text, ctx, *span)?;
                        let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            sz as u32,
                            al.trailing_zeros() as u8,
                        ));
                        let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

                        let fid = ctx.ext_fns.get("qtrt_string_concat").unwrap();
                        let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                        ctx.builder.ins().call(fref, &[sret, l_addr, r_addr]);

                        Ok(ValueKind::AggregatePtr {
                            addr: sret,
                            ty: Ty::Text,
                            slot: Some(slot),
                        })
                    }
                    BinOp::Eq => {
                        let fid = ctx.ext_fns.get("qtrt_string_eq").unwrap();
                        let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                        let call = ctx.builder.ins().call(fref, &[l_addr, r_addr]);
                        let res = ctx.builder.inst_results(call)[0];
                        Ok(ValueKind::Scalar(res, Ty::Bool))
                    }
                    BinOp::Ne => {
                        let fid = ctx.ext_fns.get("qtrt_string_eq").unwrap();
                        let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                        let call = ctx.builder.ins().call(fref, &[l_addr, r_addr]);
                        let is_eq = ctx.builder.inst_results(call)[0];
                        let res = ctx.builder.ins().bxor_imm(is_eq, 1); // !eq
                        Ok(ValueKind::Scalar(res, Ty::Bool))
                    }
                    _ => Err(eerr(*span, "unsupported operator for text")),
                }
            } else {
                // Scalar path
                let (l, lty) = expect_scalar(l_val, *span)?;
                let (r, _) = expect_scalar(r_val, *span)?;
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
        }
        Expr::Call { callee, args, span } => {
            if let Expr::Ident(fname, _) = &**callee {
                if fname == "alloc" {
                    if args.len() != 1 {
                        return Err(eerr(*span, "alloc expects 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "alloc expects positional arg")),
                    };

                    let val_kind = compile_expr(expr, ctx)?;
                    let inner_ty = match val_kind {
                        ValueKind::Scalar(_, ref t) => t.clone(),
                        ValueKind::AggregatePtr { ref ty, .. } => ty.clone(),
                    };
                    let (sz, _al) = ty_size_align(&inner_ty, ctx, *span)?;

                    let fid = ctx.ext_fns.get("qtrt_alloc").unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    let size_val = ctx.builder.ins().iconst(types::I64, sz as i64);
                    let call = ctx.builder.ins().call(func_ref, &[size_val]);
                    let ptr = ctx.builder.inst_results(call)[0];

                    match val_kind {
                        ValueKind::Scalar(v, _) => {
                            ctx.builder.ins().store(MemFlags::new(), v, ptr, 0);
                        }
                        ValueKind::AggregatePtr { addr: src, .. } => {
                            copy_memory(src, ptr, sz, ctx);
                        }
                    }

                    return Ok(ValueKind::Scalar(ptr, Ty::Ref(Box::new(inner_ty))));
                } else if fname == "dealloc" {
                    if args.len() != 1 {
                        return Err(eerr(*span, "dealloc expects 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "dealloc expects positional arg")),
                    };

                    let val_kind = compile_expr(expr, ctx)?;
                    match val_kind {
                        ValueKind::Scalar(ptr, Ty::Ref(inner)) => {
                            let (sz, _) = ty_size_align(&inner, ctx, *span)?;
                            let fid = ctx.ext_fns.get("qtrt_free").unwrap();
                            let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            let size_val = ctx.builder.ins().iconst(types::I64, sz as i64);
                            ctx.builder.ins().call(fref, &[ptr, size_val]);
                            return Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ));
                        }
                        // Keep compatibility with managed drops
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => {
                            let fid = ctx.ext_fns.get("qtrt_list_drop").unwrap();
                            let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            let elem_sz = ctx.builder.ins().iconst(types::I64, 1);
                            ctx.builder.ins().call(fref, &[addr, elem_sz]);
                            return Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ));
                        }
                        ValueKind::AggregatePtr {
                            addr,
                            ty: Ty::Array { elem },
                            ..
                        } => {
                            let (sz, _) = ty_size_align(&elem, ctx, *span)?;
                            let fid = ctx.ext_fns.get("qtrt_list_drop").unwrap();
                            let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            let elem_sz = ctx.builder.ins().iconst(types::I64, sz as i64);
                            ctx.builder.ins().call(fref, &[addr, elem_sz]);
                            return Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ));
                        }
                        _ => return Err(eerr(*span, "dealloc expects Addr<T>, text, or array")),
                    }
                } else if fname == "deref" {
                    if args.len() != 1 {
                        return Err(eerr(*span, "deref expects 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "deref expects positional arg")),
                    };

                    let ptr_kind = compile_expr(expr, ctx)?;
                    let (ptr, ty) = expect_scalar(ptr_kind, *span)?;
                    let Ty::Ref(inner) = ty else {
                        return Err(eerr(*span, "deref expects Addr<T>"));
                    };

                    if is_aggregate_ty(&inner) {
                        return Ok(ValueKind::AggregatePtr {
                            addr: ptr,
                            ty: *inner,
                            slot: None,
                        });
                    }

                    let cl_ty = ty_cl(*inner.clone(), ctx.ptr_ty);
                    let v = ctx.builder.ins().load(cl_ty, MemFlags::new(), ptr, 0);
                    return Ok(ValueKind::Scalar(v, *inner));
                } else if fname == "sys_alloc" {
                    // sys_alloc(val) -> int (ptr)
                    let arg = &args[0];
                    let expr = match arg {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "sys_alloc expects positional arg")),
                    };
                    // 1. Compile argument
                    let val_kind = compile_expr(expr, ctx)?;

                    // 2. Determine size of T
                    let ty = match val_kind {
                        ValueKind::Scalar(_, ref t) => t.clone(),
                        ValueKind::AggregatePtr { ref ty, .. } => ty.clone(),
                    };
                    let (sz, _al) = ty_size_align(&ty, ctx, *span)?;

                    // 3. Allocate memory: qtrt_alloc(size)
                    let fid = ctx.ext_fns.get("qtrt_alloc").unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    let size_val = ctx.builder.ins().iconst(types::I64, sz as i64);
                    let call = ctx.builder.ins().call(func_ref, &[size_val]);
                    let ptr = ctx.builder.inst_results(call)[0];

                    // 4. Store value to ptr
                    match val_kind {
                        ValueKind::Scalar(v, _) => {
                            let _cl_ty = ty_cl(ty.clone(), ctx.ptr_ty);
                            ctx.builder.ins().store(MemFlags::new(), v, ptr, 0);
                        }
                        ValueKind::AggregatePtr { addr: src, .. } => {
                            copy_memory(src, ptr, sz, ctx);
                        }
                    }

                    return Ok(ValueKind::Scalar(ptr, Ty::Int));
                } else if fname == "sys_dealloc" {
                    // sys_dealloc(ptr, size) or sys_dealloc(Aggregate)
                    let arg0 = args
                        .get(0)
                        .ok_or_else(|| eerr(*span, "sys_dealloc expects 1 or 2 args"))?;
                    let expr0 = match arg0 {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "arg must be positional")),
                    };
                    let val0 = compile_expr(expr0, ctx)?;

                    match val0 {
                        ValueKind::Scalar(ptr, Ty::Int) => {
                            // Needs size arg
                            let arg1 = args.get(1).ok_or_else(|| {
                                eerr(*span, "sys_dealloc(ptr) requires size argument")
                            })?;
                            let expr1 = match arg1 {
                                Arg::Pos(e) => e,
                                _ => return Err(eerr(*span, "arg must be positional")),
                            };
                            let (size_val, _) = expect_scalar(compile_expr(expr1, ctx)?, *span)?;

                            let fid = ctx.ext_fns.get("qtrt_free").unwrap();
                            let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            ctx.builder.ins().call(fref, &[ptr, size_val]);

                            return Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ));
                        }
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => {
                            let fid = ctx.ext_fns.get("qtrt_list_drop").unwrap();
                            let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            let elem_sz = ctx.builder.ins().iconst(types::I64, 1);
                            ctx.builder.ins().call(fref, &[addr, elem_sz]);
                            return Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ));
                        }
                        ValueKind::AggregatePtr {
                            addr,
                            ty: Ty::Array { elem },
                            ..
                        } => {
                            let (sz, _) = ty_size_align(&elem, ctx, *span)?;
                            let fid = ctx.ext_fns.get("qtrt_list_drop").unwrap();
                            let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            let elem_sz = ctx.builder.ins().iconst(types::I64, sz as i64);
                            ctx.builder.ins().call(fref, &[addr, elem_sz]);
                            return Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ));
                        }
                        _ => {
                            return Err(eerr(
                                *span,
                                "sys_dealloc expects int (ptr) or managed type",
                            ));
                        }
                    }
                } else if fname == "alloc_bytes" {
                    // alloc_bytes(size) -> int
                    let arg = &args[0];
                    let expr = match arg {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "alloc_bytes expects positional arg")),
                    };
                    let (size_val, _) = expect_scalar(compile_expr(expr, ctx)?, *span)?;
                    let fid = ctx.ext_fns.get("qtrt_alloc").unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    let call = ctx.builder.ins().call(func_ref, &[size_val]);
                    let ptr = ctx.builder.inst_results(call)[0];
                    return Ok(ValueKind::Scalar(ptr, Ty::Int));
                } else if fname == "ptr_get_int" {
                    // ptr_get_int(ptr, idx) -> int
                    // *(ptr + idx * 8)
                    let ptr_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 0, *span)?, ctx)?, *span)?.0;
                    let idx_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 1, *span)?, ctx)?, *span)?.0;

                    let offset = ctx.builder.ins().imul_imm(idx_val, 8);
                    let addr = ctx.builder.ins().iadd(ptr_val, offset);
                    let v = ctx.builder.ins().load(types::I64, MemFlags::new(), addr, 0);
                    return Ok(ValueKind::Scalar(v, Ty::Int));
                } else if fname == "ptr_set_int" {
                    // ptr_set_int(ptr, idx, val) -> void
                    let ptr_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 0, *span)?, ctx)?, *span)?.0;
                    let idx_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 1, *span)?, ctx)?, *span)?.0;
                    let val =
                        expect_scalar(compile_expr(get_pos_arg(args, 2, *span)?, ctx)?, *span)?.0;

                    let offset = ctx.builder.ins().imul_imm(idx_val, 8);
                    let addr = ctx.builder.ins().iadd(ptr_val, offset);
                    ctx.builder.ins().store(MemFlags::new(), val, addr, 0);
                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ));
                } else if fname == "ptr_get_byte" {
                    // Used for text or bool?
                    // ptr_get_byte(ptr, idx) -> int (byte)
                    let ptr_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 0, *span)?, ctx)?, *span)?.0;
                    let idx_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 1, *span)?, ctx)?, *span)?.0;

                    let addr = ctx.builder.ins().iadd(ptr_val, idx_val); // byte offset
                    let v = ctx.builder.ins().load(types::I8, MemFlags::new(), addr, 0);
                    let v_ext = ctx.builder.ins().uextend(types::I64, v);
                    return Ok(ValueKind::Scalar(v_ext, Ty::Int));
                } else if fname == "ptr_set_byte" {
                    // ptr_set_byte(ptr, idx, val)
                    let ptr_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 0, *span)?, ctx)?, *span)?.0;
                    let idx_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 1, *span)?, ctx)?, *span)?.0;
                    let val =
                        expect_scalar(compile_expr(get_pos_arg(args, 2, *span)?, ctx)?, *span)?.0;

                    let addr = ctx.builder.ins().iadd(ptr_val, idx_val);
                    let val_byte = ctx.builder.ins().ireduce(types::I8, val);
                    ctx.builder.ins().store(MemFlags::new(), val_byte, addr, 0);
                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ));
                } else if fname == "ptr_get_text" {
                    // ptr_get_text(ptr, idx) -> text
                    let ptr_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 0, *span)?, ctx)?, *span)?.0;
                    let idx_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 1, *span)?, ctx)?, *span)?.0;

                    let offset = ctx.builder.ins().imul_imm(idx_val, 24); // 24 bytes per text
                    let addr = ctx.builder.ins().iadd(ptr_val, offset);

                    return Ok(ValueKind::AggregatePtr {
                        addr,
                        ty: Ty::Text,
                        slot: None,
                    });
                } else if fname == "ptr_set_text" {
                    // ptr_set_text(ptr, idx, val)
                    let ptr_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 0, *span)?, ctx)?, *span)?.0;
                    let idx_val =
                        expect_scalar(compile_expr(get_pos_arg(args, 1, *span)?, ctx)?, *span)?.0;
                    let val_expr = get_pos_arg(args, 2, *span)?;
                    let val_kind = compile_expr(val_expr, ctx)?;

                    let src_addr = match val_kind {
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => addr,
                        _ => return Err(eerr(*span, "ptr_set_text expects text value")),
                    };

                    let offset = ctx.builder.ins().imul_imm(idx_val, 24);
                    let dst_addr = ctx.builder.ins().iadd(ptr_val, offset);

                    copy_memory(src_addr, dst_addr, 24, ctx);
                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ));
                }
            }

            if let Expr::Ident(fname, _) = &**callee {
                if fname == "print" || fname == "println" {
                    let wants_newline = fname == "println";
                    let arg = &args[0];
                    let expr = match arg {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, format!("{fname} expects positional arg"))),
                    };
                    let val_kind = compile_expr(expr, ctx)?;

                    match val_kind {
                        ValueKind::Scalar(v, Ty::Int) => {
                            let sym = if wants_newline {
                                "qtrt_echo_i64"
                            } else {
                                "qtrt_print_i64"
                            };
                            let fid = ctx.ext_fns.get(sym).unwrap();
                            let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            ctx.builder.ins().call(func_ref, &[v]);
                        }
                        ValueKind::Scalar(v, _) => {
                            let sym = if wants_newline {
                                "qtrt_echo_cstr"
                            } else {
                                "qtrt_print_cstr"
                            };
                            let fid = ctx.ext_fns.get(sym).unwrap();
                            let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            ctx.builder.ins().call(func_ref, &[v]);
                        }
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => {
                            let sym = if wants_newline {
                                "qtrt_print_str"
                            } else {
                                "qtrt_print_str_no_nl"
                            };
                            let fid = ctx.ext_fns.get(sym).unwrap();
                            let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                            ctx.builder.ins().call(func_ref, &[addr]);
                        }
                        _ => return Err(eerr(*span, format!("cannot {fname} this type"))),
                    }

                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ));
                }

                if fname == "read_file" {
                    let arg = &args[0];
                    let expr = match arg {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "read_file expects positional arg")),
                    };
                    let val_kind = compile_expr(expr, ctx)?;
                    let addr = match val_kind {
                        ValueKind::AggregatePtr { addr, .. } => addr,
                        _ => return Err(eerr(*span, "read_file expects text arg")),
                    };

                    let (sz, al) = ty_size_align(&Ty::Text, ctx, *span)?;
                    let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        sz as u32,
                        al.trailing_zeros() as u8,
                    ));
                    let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

                    let fid = ctx.ext_fns.get("qtrt_read_file").unwrap();
                    let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(fref, &[sret, addr]);

                    return Ok(ValueKind::AggregatePtr {
                        addr: sret,
                        ty: Ty::Text,
                        slot: Some(slot),
                    });
                }

                if fname == "write_file" {
                    if args.len() != 2 {
                        return Err(eerr(*span, "write_file expects 2 args"));
                    }
                    let path_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "write_file expects positional arg")),
                    };
                    let content_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "write_file expects positional arg")),
                    };

                    let path_val = compile_expr(path_expr, ctx)?;
                    let path_addr = match path_val {
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => addr,
                        _ => return Err(eerr(*span, "write_file expects text path")),
                    };

                    let content_val = compile_expr(content_expr, ctx)?;
                    let content_addr = match content_val {
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => addr,
                        _ => return Err(eerr(*span, "write_file expects text content")),
                    };

                    let fid = ctx.ext_fns.get("qtrt_write_file").unwrap();
                    let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(fref, &[path_addr, content_addr]);

                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ));
                }

                if fname == "sys_write" || fname == "sys_writeln" {
                    if args.len() != 1 {
                        return Err(eerr(*span, format!("{fname} expects 1 arg")));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, format!("{fname} expects positional arg"))),
                    };
                    let val_kind = compile_expr(expr, ctx)?;
                    let addr = match val_kind {
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => addr,
                        _ => return Err(eerr(*span, format!("{fname} expects text arg"))),
                    };

                    let sym = if fname == "sys_write" {
                        "qtrt_print_str_no_nl"
                    } else {
                        "qtrt_print_str"
                    };
                    let fid = ctx.ext_fns.get(sym).unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(func_ref, &[addr]);

                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ));
                }

                if fname == "sys_read_file" {
                    if args.len() != 1 {
                        return Err(eerr(*span, "sys_read_file expects 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "sys_read_file expects positional arg")),
                    };
                    let val_kind = compile_expr(expr, ctx)?;
                    let addr = match val_kind {
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => addr,
                        _ => return Err(eerr(*span, "sys_read_file expects text arg")),
                    };

                    let (sz, al) = ty_size_align(&Ty::Text, ctx, *span)?;
                    let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        sz as u32,
                        al.trailing_zeros() as u8,
                    ));
                    let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

                    let fid = ctx.ext_fns.get("qtrt_read_file").unwrap();
                    let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(fref, &[sret, addr]);

                    return Ok(ValueKind::AggregatePtr {
                        addr: sret,
                        ty: Ty::Text,
                        slot: Some(slot),
                    });
                }

                if fname == "sys_panic" {
                    if args.len() != 1 {
                        return Err(eerr(*span, "sys_panic expects 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "sys_panic expects positional arg")),
                    };
                    let msg_val = compile_expr(expr, ctx)?;
                    let addr = match msg_val {
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => addr,
                        _ => return Err(eerr(*span, "sys_panic expects text arg")),
                    };
                    let msg_ptr = ctx.builder.ins().load(ctx.ptr_ty, MemFlags::new(), addr, 0);
                    let msg_len = ctx.builder.ins().load(types::I64, MemFlags::new(), addr, 8);

                    let fid = ctx.ext_fns.get("qtrt_panic").unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(func_ref, &[msg_ptr, msg_len]);
                    ctx.builder.ins().trap(TrapCode::unwrap_user(1));

                    // `trap` terminates the current block. Switch to a fresh block so later
                    // codegen doesn't append instructions after the terminator.
                    let after = ctx.builder.create_block();
                    ctx.builder.switch_to_block(after);
                    ctx.builder.seal_block(after);

                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
                    ));
                }

                if fname == "alloc_int_array" || fname == "alloc_text_array" {
                    if args.len() != 1 {
                        return Err(eerr(*span, format!("{fname} expects 1 arg")));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, format!("{fname} expects positional arg"))),
                    };
                    let cap_kind = compile_expr(expr, ctx)?;
                    let (cap, cap_ty) = expect_scalar(cap_kind, *span)?;
                    if cap_ty != Ty::Int {
                        return Err(eerr(*span, format!("{fname} expects int cap")));
                    }

                    let elem_ty = if fname == "alloc_int_array" {
                        Ty::Int
                    } else {
                        Ty::Text
                    };
                    let (elem_sz, _) = ty_size_align(&elem_ty, ctx, *span)?;
                    let bytes = ctx.builder.ins().imul_imm(cap, elem_sz as i64);

                    let fid = ctx.ext_fns.get("qtrt_alloc").unwrap();
                    let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    let call = ctx.builder.ins().call(func_ref, &[bytes]);
                    let ptr = ctx.builder.inst_results(call)[0];

                    return Ok(ValueKind::Scalar(ptr, Ty::Ref(Box::new(elem_ty))));
                }

                if fname == "itoa_native" {
                    if args.len() != 1 {
                        return Err(eerr(*span, "itoa_native expects 1 arg"));
                    }
                    let expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "itoa_native expects positional arg")),
                    };
                    let n_kind = compile_expr(expr, ctx)?;
                    let (n, nty) = expect_scalar(n_kind, *span)?;
                    if nty != Ty::Int {
                        return Err(eerr(*span, "itoa_native expects int"));
                    }

                    let (sz, al) = ty_size_align(&Ty::Text, ctx, *span)?;
                    let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        sz as u32,
                        al.trailing_zeros() as u8,
                    ));
                    let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

                    let fid = ctx.ext_fns.get("qtrt_itoa").unwrap();
                    let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    ctx.builder.ins().call(fref, &[sret, n]);

                    return Ok(ValueKind::AggregatePtr {
                        addr: sret,
                        ty: Ty::Text,
                        slot: Some(slot),
                    });
                }

                if fname == "atoi_native" {
                    if args.len() != 2 {
                        return Err(eerr(*span, "atoi_native expects 2 args"));
                    }
                    let s_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "atoi_native expects positional args")),
                    };
                    let out_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, "atoi_native expects positional args")),
                    };

                    let s_kind = compile_expr(s_expr, ctx)?;
                    let s_addr = match s_kind {
                        ValueKind::AggregatePtr {
                            addr, ty: Ty::Text, ..
                        } => addr,
                        _ => return Err(eerr(*span, "atoi_native expects text")),
                    };

                    let out_kind = compile_expr(out_expr, ctx)?;
                    let out_ptr = match out_kind {
                        ValueKind::Scalar(p, Ty::Ref(inner)) => match *inner {
                            Ty::Int => p,
                            _ => return Err(eerr(*span, "atoi_native expects Addr<int>")),
                        },
                        ValueKind::Scalar(p, Ty::Int) => p,
                        _ => return Err(eerr(*span, "atoi_native expects Addr<int>")),
                    };

                    let fid = ctx.ext_fns.get("qtrt_atoi").unwrap();
                    let fref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                    let call = ctx.builder.ins().call(fref, &[s_addr, out_ptr]);
                    let res = ctx.builder.inst_results(call)[0];
                    return Ok(ValueKind::Scalar(res, Ty::Bool));
                }

                if fname == "get_int"
                    || fname == "set_int"
                    || fname == "get_text"
                    || fname == "set_text"
                {
                    let wants_set = fname.starts_with("set_");
                    let wants_text = fname.ends_with("text");
                    let elem_ty = if wants_text { Ty::Text } else { Ty::Int };
                    let expected_args = if wants_set { 3 } else { 2 };
                    if args.len() != expected_args {
                        return Err(eerr(*span, format!("{fname} expects {expected_args} args")));
                    }

                    let p_expr = match &args[0] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, format!("{fname} expects positional args"))),
                    };
                    let i_expr = match &args[1] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, format!("{fname} expects positional args"))),
                    };

                    let ptr_kind = compile_expr(p_expr, ctx)?;
                    let base_ptr = match ptr_kind {
                        ValueKind::Scalar(v, Ty::Int) | ValueKind::Scalar(v, Ty::Ref(_)) => v,
                        _ => return Err(eerr(*span, format!("{fname} expects pointer value"))),
                    };

                    // Type check removed as we don't have inner_ty

                    let idx_kind = compile_expr(i_expr, ctx)?;
                    let (idx, idx_ty) = expect_scalar(idx_kind, *span)?;
                    if idx_ty != Ty::Int {
                        return Err(eerr(*span, format!("{fname} index must be int")));
                    }

                    let (elem_sz, elem_al) = ty_size_align(&elem_ty, ctx, *span)?;
                    let offset = ctx.builder.ins().imul_imm(idx, elem_sz as i64);
                    let elem_addr = ctx.builder.ins().iadd(base_ptr, offset);

                    if !wants_set {
                        // get_*
                        if wants_text {
                            let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                                StackSlotKind::ExplicitSlot,
                                elem_sz as u32,
                                elem_al.trailing_zeros() as u8,
                            ));
                            let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);
                            copy_memory(elem_addr, sret, elem_sz, ctx);
                            return Ok(ValueKind::AggregatePtr {
                                addr: sret,
                                ty: Ty::Text,
                                slot: Some(slot),
                            });
                        } else {
                            let cl_ty = ty_cl(Ty::Int, ctx.ptr_ty);
                            let v = ctx.builder.ins().load(cl_ty, MemFlags::new(), elem_addr, 0);
                            return Ok(ValueKind::Scalar(v, Ty::Int));
                        }
                    }

                    // set_*
                    let v_expr = match &args[2] {
                        Arg::Pos(e) => e,
                        _ => return Err(eerr(*span, format!("{fname} expects positional args"))),
                    };
                    let val_kind = compile_expr(v_expr, ctx)?;
                    if wants_text {
                        let src_addr = match val_kind {
                            ValueKind::AggregatePtr {
                                addr, ty: Ty::Text, ..
                            } => addr,
                            _ => return Err(eerr(*span, "set_text expects text value")),
                        };
                        copy_memory(src_addr, elem_addr, elem_sz, ctx);
                    } else {
                        let (v, vty) = expect_scalar(val_kind, *span)?;
                        if vty != Ty::Int {
                            return Err(eerr(*span, "set_int expects int value"));
                        }
                        ctx.builder.ins().store(MemFlags::new(), v, elem_addr, 0);
                    }

                    return Ok(ValueKind::Scalar(
                        ctx.builder.ins().iconst(types::I64, 0),
                        Ty::Void,
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
                    Expr::Ident(name, _) => ctx.sem.methods.contains_key(name),
                    _ => false,
                };

                if treat_as_type {
                    let Expr::Ident(type_name, _) = &**base else {
                        unreachable!();
                    };
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

                    let ret_ty = msig.ret.clone();
                    let mut arg_vals = build_call_args(args, &msig.params, *span, ctx)?;

                    if ret_ty != Ty::Void && is_aggregate_ty(&ret_ty) {
                        let (sz, al) = ty_size_align(&ret_ty, ctx, *span)?;
                        let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            sz as u32,
                            al.trailing_zeros() as u8,
                        ));
                        let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);
                        let mut full_args = vec![sret];
                        full_args.append(&mut arg_vals);
                        ctx.builder.ins().call(func_ref, &full_args);
                        Ok(ValueKind::AggregatePtr {
                            addr: sret,
                            ty: ret_ty,
                            slot: Some(slot),
                        })
                    } else {
                        let call = ctx.builder.ins().call(func_ref, &arg_vals);
                        let results = ctx.builder.inst_results(call);
                        if results.is_empty() {
                            Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ))
                        } else {
                            Ok(ValueKind::Scalar(results[0], ret_ty))
                        }
                    }
                } else {
                    let recv_val = compile_expr(base, ctx)?;

                    // Hard-coded unwrap/expect for intrinsic Option/Result.
                    // Final surface names: unwrap/expect (not reserved keywords).
                    let is_unwrap = matches!(field.as_str(), "unwrap");
                    let is_expect = matches!(field.as_str(), "expect");
                    if is_unwrap || is_expect {
                        if let ValueKind::AggregatePtr {
                            addr: recv_addr,
                            ty: Ty::Enum(enum_name),
                            ..
                        } = &recv_val
                        {
                            let is_option = enum_name.starts_with("Option<");
                            let is_result = enum_name.starts_with("Result<");
                            if is_option || is_result {
                                if is_unwrap {
                                    if !args.is_empty() {
                                        return Err(eerr(*span, "unwrap takes no args"));
                                    }
                                } else {
                                    if args.len() != 1 {
                                        return Err(eerr(*span, "expect takes exactly 1 arg"));
                                    }
                                    if !matches!(&args[0], Arg::Pos(_)) {
                                        return Err(eerr(*span, "expect does not take named args"));
                                    }
                                }

                                let einfo = ctx
                                    .sem
                                    .enums
                                    .get(enum_name)
                                    .ok_or_else(|| eerr(*span, "unknown enum"))?
                                    .clone();

                                let ok_variant = if is_option { "Some" } else { "Ok" };
                                let ok_fields = einfo
                                    .variants
                                    .get(ok_variant)
                                    .ok_or_else(|| eerr(*span, "malformed intrinsic enum"))?;
                                if ok_fields.len() != 1 {
                                    return Err(eerr(*span, "malformed intrinsic enum"));
                                }
                                let ok_ty = ok_fields[0].clone();
                                let ok_tag = enum_tag_index(&einfo, ok_variant, *span)?;

                                let ok_block = ctx.builder.create_block();
                                let fail_block = ctx.builder.create_block();
                                let cont_block = ctx.builder.create_block();

                                let cont_param_ty = if is_aggregate_ty(&ok_ty) {
                                    ctx.ptr_ty
                                } else {
                                    ty_cl(ok_ty.clone(), ctx.ptr_ty)
                                };
                                ctx.builder.append_block_param(cont_block, cont_param_ty);

                                let tag = ctx.builder.ins().load(
                                    types::I64,
                                    MemFlags::new(),
                                    *recv_addr,
                                    0,
                                );
                                let ok_tag_val = ctx.builder.ins().iconst(types::I64, ok_tag);
                                let is_ok = ctx.builder.ins().icmp(IntCC::Equal, tag, ok_tag_val);
                                ctx.builder
                                    .ins()
                                    .brif(is_ok, ok_block, &[], fail_block, &[]);

                                // ok: extract payload
                                ctx.builder.switch_to_block(ok_block);
                                ctx.builder.seal_block(ok_block);
                                let ok_off =
                                    enum_variant_field_offset(&einfo, ok_variant, 0, ctx, *span)?;
                                if is_aggregate_ty(&ok_ty) {
                                    let ptr = ctx.builder.ins().iadd_imm(*recv_addr, ok_off as i64);
                                    ctx.builder.ins().jump(cont_block, &[BlockArg::from(ptr)]);
                                } else {
                                    let cl_ty = ty_cl(ok_ty.clone(), ctx.ptr_ty);
                                    let v = ctx.builder.ins().load(
                                        cl_ty,
                                        MemFlags::new(),
                                        *recv_addr,
                                        ok_off,
                                    );
                                    ctx.builder.ins().jump(cont_block, &[BlockArg::from(v)]);
                                }

                                // fail: panic with message
                                ctx.builder.switch_to_block(fail_block);
                                ctx.builder.seal_block(fail_block);

                                let (msg_ptr, msg_len) = if is_expect {
                                    let Arg::Pos(msg_expr) = &args[0] else {
                                        unreachable!();
                                    };
                                    let msg_val = compile_expr(msg_expr, ctx)?;
                                    match msg_val {
                                        ValueKind::AggregatePtr {
                                            addr, ty: Ty::Text, ..
                                        } => {
                                            let ptr = ctx.builder.ins().load(
                                                ctx.ptr_ty,
                                                MemFlags::new(),
                                                addr,
                                                0,
                                            );
                                            let len = ctx.builder.ins().load(
                                                types::I64,
                                                MemFlags::new(),
                                                addr,
                                                8,
                                            );
                                            (ptr, len)
                                        }
                                        _ => return Err(eerr(*span, "expect(msg) requires text")),
                                    }
                                } else {
                                    let data_id = ctx
                                        .str_data_ids
                                        .get("unwrap failed")
                                        .ok_or_else(|| eerr(*span, "missing builtin string"))?;
                                    let global_val =
                                        ctx.module.declare_data_in_func(*data_id, ctx.builder.func);
                                    let ptr =
                                        ctx.builder.ins().global_value(ctx.ptr_ty, global_val);
                                    let len = ctx
                                        .builder
                                        .ins()
                                        .iconst(types::I64, "unwrap failed".len() as i64);
                                    (ptr, len)
                                };

                                let fid = ctx.ext_fns.get("qtrt_panic").unwrap();
                                let func_ref =
                                    ctx.module.declare_func_in_func(*fid, ctx.builder.func);
                                ctx.builder.ins().call(func_ref, &[msg_ptr, msg_len]);
                                ctx.builder.ins().trap(TrapCode::unwrap_user(1));

                                // continuation
                                ctx.builder.switch_to_block(cont_block);
                                ctx.builder.seal_block(cont_block);
                                let v = ctx.builder.block_params(cont_block)[0];
                                return if is_aggregate_ty(&ok_ty) {
                                    Ok(ValueKind::AggregatePtr {
                                        addr: v,
                                        ty: ok_ty,
                                        slot: None,
                                    })
                                } else {
                                    Ok(ValueKind::Scalar(v, ok_ty))
                                };
                            }
                        }
                    }

                    let (sname, recv_ptr) = match recv_val {
                        ValueKind::AggregatePtr {
                            addr,
                            ty: Ty::Struct(name),
                            ..
                        } => (name, addr),
                        ValueKind::Scalar(v, Ty::Ref(inner)) => match *inner {
                            Ty::Struct(name) => (name, v),
                            _ => {
                                return Err(eerr(
                                    *field_span,
                                    "method call requires struct receiver",
                                ));
                            }
                        },
                        ValueKind::Scalar(v, Ty::Struct(name)) => (name, v),
                        _ => return Err(eerr(*field_span, "method call requires struct receiver")),
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

                    let ret_ty = msig.ret.clone();
                    let mut rest = build_call_args(args, &msig.params[1..], *span, ctx)?;

                    if ret_ty != Ty::Void && is_aggregate_ty(&ret_ty) {
                        let (sz, al) = ty_size_align(&ret_ty, ctx, *span)?;
                        let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                            StackSlotKind::ExplicitSlot,
                            sz as u32,
                            al.trailing_zeros() as u8,
                        ));
                        let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

                        let mut arg_vals = vec![sret, recv_ptr];
                        arg_vals.append(&mut rest);
                        ctx.builder.ins().call(func_ref, &arg_vals);

                        Ok(ValueKind::AggregatePtr {
                            addr: sret,
                            ty: ret_ty,
                            slot: Some(slot),
                        })
                    } else {
                        let mut arg_vals = vec![recv_ptr];
                        arg_vals.append(&mut rest);

                        let call = ctx.builder.ins().call(func_ref, &arg_vals);
                        let results = ctx.builder.inst_results(call);
                        if results.is_empty() {
                            Ok(ValueKind::Scalar(
                                ctx.builder.ins().iconst(types::I64, 0),
                                Ty::Void,
                            ))
                        } else {
                            Ok(ValueKind::Scalar(results[0], ret_ty))
                        }
                    }
                }
            } else if let Expr::Ident(fname, _) = &**callee {
                // Struct construction via parens: `TypeName(...)`
                // Semantics allows this as an alternative to `TypeName { field: ... }`.
                if ctx.sem.structs.contains_key(fname) {
                    let layout = get_struct_layout(fname, ctx, *span)?;
                    let align_shift = layout.align.trailing_zeros() as u8;
                    let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        layout.size as u32,
                        align_shift,
                    ));
                    let base = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

                    if args.iter().all(|a| matches!(a, Arg::Pos(_))) {
                        if args.len() != layout.fields.len() {
                            return Err(eerr(
                                *span,
                                format!(
                                    "struct {fname} expects {} args, got {}",
                                    layout.fields.len(),
                                    args.len()
                                ),
                            ));
                        }
                        for (i, a) in args.iter().enumerate() {
                            let Arg::Pos(expr) = a else { unreachable!() };
                            let fld = &layout.fields[i];
                            let val = compile_expr(expr, ctx)?;
                            match val {
                                ValueKind::Scalar(v, _) => {
                                    store_field(base, fld.offset, v, &fld.ty, ctx)?
                                }
                                ValueKind::AggregatePtr { addr: src, ty, .. } => {
                                    let dst = ctx.builder.ins().iadd_imm(base, fld.offset as i64);
                                    let (sz, _) = ty_size_align(&ty, ctx, *span)?;
                                    copy_memory(src, dst, sz, ctx);
                                }
                            }
                        }
                    } else if args.iter().all(|a| matches!(a, Arg::Named { .. })) {
                        let mut seen: HashSet<&str> = HashSet::new();
                        for a in args {
                            let Arg::Named {
                                name,
                                expr,
                                span: nspan,
                            } = a
                            else {
                                unreachable!();
                            };
                            let fld =
                                layout.fields.iter().find(|f| f.name == *name).ok_or_else(
                                    || eerr(*nspan, format!("unknown field: {name}")),
                                )?;
                            if !seen.insert(name.as_str()) {
                                return Err(eerr(*nspan, format!("duplicate field: {name}")));
                            }
                            let val = compile_expr(expr, ctx)?;
                            match val {
                                ValueKind::Scalar(v, _) => {
                                    store_field(base, fld.offset, v, &fld.ty, ctx)?
                                }
                                ValueKind::AggregatePtr { addr: src, ty, .. } => {
                                    let dst = ctx.builder.ins().iadd_imm(base, fld.offset as i64);
                                    let (sz, _) = ty_size_align(&ty, ctx, *span)?;
                                    copy_memory(src, dst, sz, ctx);
                                }
                            }
                        }
                        if seen.len() != layout.fields.len() {
                            return Err(eerr(*span, "missing field in struct constructor"));
                        }
                    } else {
                        return Err(eerr(
                            *span,
                            "struct construction args must be either all positional or all named",
                        ));
                    }

                    return Ok(ValueKind::AggregatePtr {
                        addr: base,
                        ty: Ty::Struct(fname.clone()),
                        slot: Some(slot),
                    });
                }

                let fid = ctx
                    .user_fns
                    .get(fname)
                    .ok_or_else(|| eerr(*span, "unknown func"))?;
                let func_ref = ctx.module.declare_func_in_func(*fid, ctx.builder.func);

                let sig = ctx.sem.fns.get(fname).unwrap();
                let ret_ty = sig.ret.clone();
                let mut arg_vals = build_call_args(args, &sig.params, *span, ctx)?;

                if ret_ty != Ty::Void && is_aggregate_ty(&ret_ty) {
                    let (sz, al) = ty_size_align(&ret_ty, ctx, *span)?;
                    let slot = ctx.builder.create_sized_stack_slot(StackSlotData::new(
                        StackSlotKind::ExplicitSlot,
                        sz as u32,
                        al.trailing_zeros() as u8,
                    ));
                    let sret = ctx.builder.ins().stack_addr(ctx.ptr_ty, slot, 0);

                    let mut full_args = vec![sret];
                    full_args.append(&mut arg_vals);
                    ctx.builder.ins().call(func_ref, &full_args);

                    Ok(ValueKind::AggregatePtr {
                        addr: sret,
                        ty: ret_ty,
                        slot: Some(slot),
                    })
                } else {
                    let call = ctx.builder.ins().call(func_ref, &arg_vals);
                    let results = ctx.builder.inst_results(call);
                    if results.is_empty() {
                        Ok(ValueKind::Scalar(
                            ctx.builder.ins().iconst(types::I64, 0),
                            Ty::Void,
                        ))
                    } else {
                        Ok(ValueKind::Scalar(results[0], ret_ty))
                    }
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
        for (k, (_, arg_span)) in &named_map {
            if !param_names.contains(k) {
                return Err(eerr(*arg_span, format!("unknown parameter: {k}")));
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
                    ValueKind::AggregatePtr {
                        addr,
                        ty: Ty::Struct(ref n),
                        ..
                    } => {
                        if n != sname {
                            return Err(eerr(*arg_sp, "struct arg type mismatch"));
                        }
                        addr
                    }
                    ValueKind::Scalar(_, _) => return Err(eerr(*arg_sp, "expected struct arg")),
                    _ => return Err(eerr(*arg_sp, "expected struct arg")),
                };
                arg_vals.push(ptr);
            }
            Ty::Enum(_) | Ty::Array { .. } | Ty::Text => {
                let ptr = match compile_expr(expr_ref, ctx)? {
                    ValueKind::AggregatePtr { addr, ty, .. } => {
                        if &ty != expected_ty {
                            return Err(eerr(*arg_sp, "aggregate arg type mismatch"));
                        }
                        addr
                    }
                    _ => return Err(eerr(*arg_sp, "expected aggregate arg")),
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
        ValueKind::AggregatePtr { .. } => Err(eerr(span, "expected scalar value")),
    }
}

fn get_pos_arg<'a>(args: &'a [Arg], idx: usize, span: Span) -> Result<&'a Expr, EmitError> {
    let arg = args.get(idx).ok_or_else(|| eerr(span, "missing arg"))?;
    match arg {
        Arg::Pos(e) => Ok(e),
        Arg::Named { .. } => Err(eerr(span, "expected positional args")),
    }
}

fn ty_cl(t: Ty, ptr_ty: Type) -> Type {
    match t {
        Ty::Int => types::I64,
        Ty::Bool => types::I8,
        Ty::Text => ptr_ty,
        Ty::Enum(_) => ptr_ty,
        Ty::Struct(_) => ptr_ty,
        Ty::Array { .. } => ptr_ty,
        Ty::Ref(_) => ptr_ty,
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
    for (fname, fty, _) in &sinfo.fields {
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
        Ty::Text => (ptr_bytes * 3, ptr_bytes),
        Ty::Struct(name) => {
            let layout = get_struct_layout(name, ctx, span)?;
            (layout.size, layout.align)
        }
        Ty::Enum(name) => {
            // Enum layout: tag (i64) + payload
            // Payload size = max(variant_size)
            // Payload align = max(variant_align)
            // Total alignment = max(8, payload_align) (tag is i64)
            // Total size = align_up(8 + payload_size, total_align)

            // We need to look up enum info.
            // We can't easily get it from `ctx.sem` inside this match without cloning or heavy lookup?
            // checking `ctx.sem.enums`.
            let einfo = ctx
                .sem
                .enums
                .get(name)
                .ok_or_else(|| eerr(span, "unknown enum"))?;

            let mut max_payload_size = 0;
            let mut max_payload_align = 1;

            for (_vname, fields) in &einfo.variants {
                // Calculate size of this variant's payload (tuple of fields)
                // Fields are laid out sequentially (C-struct style)
                let mut v_size = 0;
                let mut v_align = 1;
                for fty in fields {
                    let (sz, al) = ty_size_align(fty, ctx, span)?;
                    v_size = align_up(v_size, al);
                    v_align = v_align.max(al);
                    v_size += sz;
                }
                v_size = align_up(v_size, v_align); // padding at end?

                max_payload_size = max_payload_size.max(v_size);
                max_payload_align = max_payload_align.max(v_align);
            }

            let tag_size = 8; // i64
            let tag_align = 8;

            let total_align = tag_align.max(max_payload_align);
            // Layout: Tag (0..8) | Padding? | Payload
            // Payload offset = align_up(8, max_payload_align)
            // But we simplify: Tag is at 0. Payload starts at 8 (assuming align <= 8).
            // Actually payload start must be aligned.

            let payload_offset = align_up(tag_size, max_payload_align);
            let total_size = align_up(payload_offset + max_payload_size, total_align);

            (total_size, total_align)
        }
        Ty::Array { elem: _ } => {
            // FatPtr size = 3 * ptr_bytes (ptr, len, cap)
            (ptr_bytes * 3, ptr_bytes)
        }
        Ty::Ref(_) => (ptr_bytes, ptr_bytes),
        Ty::Void => return Err(eerr(span, "void has no size")),
    })
}

fn is_aggregate_ty(ty: &Ty) -> bool {
    matches!(
        ty,
        Ty::Struct(_) | Ty::Enum(_) | Ty::Array { .. } | Ty::Text
    )
}

fn enum_tag_index(
    einfo: &crate::sem::EnumInfo,
    variant: &str,
    span: Span,
) -> Result<i64, EmitError> {
    let mut sorted: Vec<_> = einfo.variants.keys().collect();
    sorted.sort();
    let idx = sorted
        .iter()
        .position(|k| k.as_str() == variant)
        .ok_or_else(|| eerr(span, "unknown variant"))?;
    Ok(idx as i64)
}

fn enum_payload_base_offset(
    einfo: &crate::sem::EnumInfo,
    ctx: &mut CompilerCtx,
    span: Span,
) -> Result<i32, EmitError> {
    let mut max_payload_align = 1;
    for (_name, fields) in &einfo.variants {
        let mut v_align = 1;
        for fty in fields {
            let (_, al) = ty_size_align(fty, ctx, span)?;
            v_align = v_align.max(al);
        }
        max_payload_align = max_payload_align.max(v_align);
    }
    Ok(align_up(8, max_payload_align))
}

fn enum_variant_field_offset(
    einfo: &crate::sem::EnumInfo,
    variant: &str,
    field_index: usize,
    ctx: &mut CompilerCtx,
    span: Span,
) -> Result<i32, EmitError> {
    let fields = einfo
        .variants
        .get(variant)
        .ok_or_else(|| eerr(span, "unknown variant"))?;
    if field_index >= fields.len() {
        return Err(eerr(span, "field index out of range"));
    }

    let payload_base = enum_payload_base_offset(einfo, ctx, span)?;
    let mut current = payload_base;
    for (i, fty) in fields.iter().enumerate() {
        let (sz, al) = ty_size_align(fty, ctx, span)?;
        current = align_up(current, al);
        if i == field_index {
            return Ok(current);
        }
        current += sz;
    }
    Err(eerr(span, "field index out of range"))
}

fn copy_memory(src: Value, dst: Value, size: i32, ctx: &mut CompilerCtx) {
    ctx.builder.emit_small_memory_copy(
        ctx.module.target_config(),
        dst,
        src,
        size as u64,
        0,    // align unknown or 1
        0,    // align unknown or 1
        true, // non-overlapping
        MemFlags::new(),
    );
}

fn store_elem(
    base: Value,
    idx: i32,
    val: Value,
    elem_ty: &Ty,
    elem_size: i32,
    ctx: &mut CompilerCtx,
) -> Result<(), EmitError> {
    let offset = idx * elem_size;
    let addr = ctx.builder.ins().iadd_imm(base, offset as i64);
    match elem_ty {
        Ty::Int | Ty::Bool => {
            ctx.builder.ins().store(MemFlags::new(), val, addr, 0);
        }
        _ => {
            copy_memory(val, addr, elem_size, ctx);
        }
    }
    Ok(())
}

fn align_up(n: i32, align: i32) -> i32 {
    if align <= 1 {
        return n;
    }
    let rem = n % align;
    if rem == 0 { n } else { n + (align - rem) }
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
        Stmt::Foreach { iter, body, .. } => {
            collect_strings_expr(iter, map, next);
            for s in body {
                collect_strings_stmt(s, map, next);
            }
        }
        Stmt::Case {
            scrutinee, arms, ..
        } => {
            collect_strings_expr(scrutinee, map, next);
            for (_pat, body) in arms {
                for s in body {
                    collect_strings_stmt(s, map, next);
                }
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
        Expr::Try { expr, .. } => collect_strings_expr(expr, map, next),
        Expr::Index { base, index, .. } => {
            collect_strings_expr(base, map, next);
            collect_strings_expr(index, map, next);
        }
        Expr::Field { base, .. } => collect_strings_expr(base, map, next),
        Expr::ArrayLit { elements, .. } => {
            for e in elements {
                collect_strings_expr(e, map, next);
            }
        }
        Expr::EnumLit { args, .. } => {
            for e in args {
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
