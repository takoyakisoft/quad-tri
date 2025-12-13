use crate::ast::*;
use crate::lex::{Kw, Span, TokKind, Token};

#[derive(Debug)]
pub struct ParseError {
    pub span: Span,
    pub msg: String,
}
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.line, self.span.col, self.msg)
    }
}
impl std::error::Error for ParseError {}
fn perr(span: Span, msg: impl Into<String>) -> ParseError {
    ParseError {
        span,
        msg: msg.into(),
    }
}

pub fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut p = Parser { tokens, i: 0 };
    p.skip_newlines();
    let mut imports = Vec::new();
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    // Parse imports first
    while !p.at_eof() {
        if let TokKind::Kw(Kw::From) = p.peek().kind {
            imports.push(p.parse_import()?);
            p.skip_newlines();
        } else {
            break;
        }
    }

    // Then definitions and implementations (type/impl/func)
    while !p.at_eof() {
        match p.peek().kind {
            TokKind::Kw(Kw::Type) => {
                structs.push(p.parse_struct()?);
                p.skip_newlines();
            }
            TokKind::Kw(Kw::Impl) => {
                funcs.extend(p.parse_impl()?);
                p.skip_newlines();
            }
            TokKind::Kw(Kw::Func) => {
                funcs.push(p.parse_func(None)?);
                p.skip_newlines();
            }
            TokKind::Eof => break,
            _ => {
                let t = p.peek();
                return Err(perr(
                    t.span,
                    format!("expected type/impl/func, got {:?}", t.kind),
                ));
            }
        }
    }
    Ok(Program {
        imports,
        structs,
        funcs,
    })
}

struct Parser<'a> {
    tokens: &'a [Token],
    i: usize,
}
impl<'a> Parser<'a> {
    fn at_eof(&self) -> bool {
        matches!(self.peek().kind, TokKind::Eof)
    }
    fn peek(&self) -> &'a Token {
        self.tokens
            .get(self.i)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }
    fn peek_n(&self, n: usize) -> &'a Token {
        self.tokens
            .get(self.i + n)
            .unwrap_or_else(|| self.tokens.last().unwrap())
    }
    fn next(&mut self) -> &'a Token {
        let t = self.peek();
        self.i = (self.i + 1).min(self.tokens.len());
        t
    }

    fn skip_newlines(&mut self) {
        while matches!(self.peek().kind, TokKind::Newline) {
            self.next();
        }
    }

    fn expect(&mut self, kind: TokKind) -> Result<Span, ParseError> {
        let t = self.peek();
        if t.kind == kind {
            let s = t.span;
            self.next();
            Ok(s)
        } else {
            Err(perr(
                t.span,
                format!("expected {:?}, got {:?}", kind, t.kind),
            ))
        }
    }
    fn expect_kw(&mut self, kw: Kw) -> Result<Span, ParseError> {
        let t = self.peek();
        match &t.kind {
            TokKind::Kw(k) if *k == kw => {
                let s = t.span;
                self.next();
                Ok(s)
            }
            _ => Err(perr(
                t.span,
                format!("expected keyword {:?}, got {:?}", kw, t.kind),
            )),
        }
    }
    fn take_ident(&mut self) -> Result<(String, Span), ParseError> {
        let t = self.peek();
        match &t.kind {
            TokKind::Ident(s) => {
                let sp = t.span;
                let name = s.clone();
                self.next();
                Ok((name, sp))
            }
            _ => Err(perr(
                t.span,
                format!("expected identifier, got {:?}", t.kind),
            )),
        }
    }

    fn take_str(&mut self) -> Result<(String, Span), ParseError> {
        let t = self.peek();
        match &t.kind {
            TokKind::Str(s) => {
                let sp = t.span;
                let v = s.clone();
                self.next();
                Ok((v, sp))
            }
            _ => Err(perr(
                t.span,
                format!("expected string literal, got {:?}", t.kind),
            )),
        }
    }

    fn parse_import(&mut self) -> Result<Import, ParseError> {
        self.expect_kw(Kw::From)?;
        let (path, sp) = self.take_str()?;
        self.expect(TokKind::Newline)?;
        Ok(Import { path, span: sp })
    }

    fn parse_struct(&mut self) -> Result<StructDef, ParseError> {
        let sp = self.expect_kw(Kw::Type)?;
        let (name, _) = self.take_ident()?;
        self.expect(TokKind::LBrace)?;
        self.skip_newlines();

        let mut fields = Vec::new();
        if self.peek().kind != TokKind::RBrace {
            loop {
                let (fname, fsp) = self.take_ident()?;
                self.expect(TokKind::Colon)?;
                let (fty, _) = self.take_ident()?;
                fields.push(Param {
                    name: fname,
                    ty: fty,
                    mutable: false,
                    span: fsp,
                });

                if self.peek().kind == TokKind::Comma {
                    self.next();
                    self.skip_newlines();
                    continue;
                }
                break;
            }
        }

        self.expect(TokKind::RBrace)?;
        self.expect(TokKind::Newline)?;

        Ok(StructDef {
            name,
            fields,
            span: sp,
        })
    }

    fn parse_impl(&mut self) -> Result<Vec<Func>, ParseError> {
        self.expect_kw(Kw::Impl)?;
        let (ty_name, _) = self.take_ident()?;
        self.expect(TokKind::Colon)?;
        self.expect(TokKind::Newline)?;
        self.expect(TokKind::Indent)?;
        self.skip_newlines();

        let mut funcs = Vec::new();
        while self.peek().kind != TokKind::Dedent {
            funcs.push(self.parse_func(Some(&ty_name))?);
            self.skip_newlines();
        }
        self.expect(TokKind::Dedent)?;
        Ok(funcs)
    }

    fn parse_func(&mut self, impl_target: Option<&str>) -> Result<Func, ParseError> {
        let func_span = self.expect_kw(Kw::Func)?;
        let (name, _) = self.take_ident()?;

        self.expect(TokKind::LParen)?;
        let params = self.parse_params(impl_target)?;
        self.expect(TokKind::RParen)?;

        let mut ret_ty: Option<String> = None;
        if self.peek().kind == TokKind::Arrow {
            self.next();
            let (ty, _) = self.take_ident()?;
            ret_ty = Some(ty);
        }

        self.expect(TokKind::Colon)?;
        self.expect(TokKind::Newline)?;
        let body = self.parse_block()?;

        let method = impl_target.map(|ty| MethodInfo {
            type_name: ty.to_string(),
            method_name: name.clone(),
            has_self: params
                .first()
                .map(|p| p.name == "self" || p.name == "slf")
                .unwrap_or(false),
        });
        let full_name = if let Some(info) = &method {
            format!("{}__{}", info.type_name, name)
        } else {
            name.clone()
        };
        Ok(Func {
            method,
            name: full_name,
            params,
            ret_ty,
            body,
            span: func_span,
        })
    }

    fn parse_params(&mut self, impl_target: Option<&str>) -> Result<Vec<Param>, ParseError> {
        let mut out = Vec::new();
        if self.peek().kind == TokKind::RParen {
            return Ok(out);
        }
        loop {
            let t = self.peek();
            if out.is_empty() {
                if matches!(t.kind, TokKind::Kw(Kw::Self_)) {
                    let sp = t.span;
                    self.next();
                    let ty = impl_target
                        .ok_or_else(|| perr(sp, "self is only allowed inside impl"))?
                        .to_string();
                    out.push(Param {
                        name: "self".to_string(),
                        ty,
                        mutable: true,
                        span: sp,
                    });
                    if self.peek().kind == TokKind::Comma {
                        self.next();
                        continue;
                    }
                    if self.peek().kind == TokKind::RParen {
                        break;
                    }
                }
            }

            let (name, sp) = self.take_ident()?;
            self.expect(TokKind::Colon)?;
            let (ty, _) = self.take_ident()?;
            out.push(Param {
                name,
                ty,
                mutable: false,
                span: sp,
            });

            if self.peek().kind == TokKind::Comma {
                self.next();
                continue;
            }
            break;
        }
        Ok(out)
    }

    fn parse_block(&mut self) -> Result<Vec<Stmt>, ParseError> {
        self.expect(TokKind::Indent)?;
        self.skip_newlines();
        let mut body = Vec::new();
        while self.peek().kind != TokKind::Dedent && !self.at_eof() {
            body.push(self.parse_stmt()?);
            self.skip_newlines();
        }
        self.expect(TokKind::Dedent)?;
        Ok(body)
    }

    fn parse_stmt(&mut self) -> Result<Stmt, ParseError> {
        let t = self.peek();
        match &t.kind {
            TokKind::Kw(Kw::Lock) | TokKind::Kw(Kw::Vars) => self.parse_vardecl(),
            TokKind::Kw(Kw::When) => self.parse_if(),
            TokKind::Kw(Kw::Loop) => self.parse_while(),
            TokKind::Kw(Kw::Stop) => {
                let sp = t.span;
                self.next();
                self.expect(TokKind::Newline)?;
                Ok(Stmt::Break { span: sp })
            }
            TokKind::Kw(Kw::Next) => {
                let sp = t.span;
                self.next();
                self.expect(TokKind::Newline)?;
                Ok(Stmt::Continue { span: sp })
            }
            TokKind::Kw(Kw::Back) => self.parse_back(),

            // assignment starts with Ident then := (optionally via .field)
            TokKind::Ident(_)
                if (self.peek_n(1).kind == TokKind::Assign
                    || (self.peek_n(1).kind == TokKind::Dot
                        && self.peek_n(3).kind == TokKind::Assign)) =>
            {
                self.parse_assign()
            }

            // otherwise: expression statement (echo(...), foo(...), etc.)
            _ => {
                let sp = t.span;
                let expr = self.parse_expr(0)?;
                self.expect(TokKind::Newline)?;
                Ok(Stmt::Expr { expr, span: sp })
            }
        }
    }

    fn parse_vardecl(&mut self) -> Result<Stmt, ParseError> {
        let t = self.peek();
        let (mutable, sp) = match &t.kind {
            TokKind::Kw(Kw::Vars) => (true, t.span),
            TokKind::Kw(Kw::Lock) => (false, t.span),
            _ => return Err(perr(t.span, "expected vars/lock")),
        };
        self.next();

        let (name, _) = self.take_ident()?;
        self.expect(TokKind::Colon)?;
        let (ty, _) = self.take_ident()?;
        self.expect(TokKind::Assign)?; // :=
        let init = self.parse_expr(0)?;
        self.expect(TokKind::Newline)?;
        Ok(Stmt::VarDecl {
            mutable,
            name,
            ty,
            init,
            span: sp,
        })
    }

    fn parse_assign(&mut self) -> Result<Stmt, ParseError> {
        let (name, sp) = self.take_ident()?;
        let target = if self.peek().kind == TokKind::Dot {
            self.next();
            let (field, _) = self.take_ident()?;
            AssignTarget::Field { base: name, field }
        } else {
            AssignTarget::Name(name)
        };
        self.expect(TokKind::Assign)?;
        let expr = self.parse_expr(0)?;
        self.expect(TokKind::Newline)?;
        Ok(Stmt::Assign {
            target,
            expr,
            span: sp,
        })
    }

    fn parse_back(&mut self) -> Result<Stmt, ParseError> {
        let t = self.peek();
        let sp = t.span;
        self.next();
        let expr = if self.peek().kind == TokKind::Newline {
            None
        } else {
            Some(self.parse_expr(0)?)
        };
        self.expect(TokKind::Newline)?;
        Ok(Stmt::Back { expr, span: sp })
    }

    fn parse_if(&mut self) -> Result<Stmt, ParseError> {
        let sp = self.expect_kw(Kw::When)?;
        let cond = self.parse_expr(0)?;
        self.expect(TokKind::Colon)?;
        self.expect(TokKind::Newline)?;
        let body = self.parse_block()?;

        let mut arms = vec![(cond, body)];
        self.skip_newlines();

        while matches!(self.peek().kind, TokKind::Kw(Kw::Elif)) {
            self.next();
            let c = self.parse_expr(0)?;
            self.expect(TokKind::Colon)?;
            self.expect(TokKind::Newline)?;
            let b = self.parse_block()?;
            arms.push((c, b));
            self.skip_newlines();
        }

        let else_body = if matches!(self.peek().kind, TokKind::Kw(Kw::Else)) {
            self.next();
            self.expect(TokKind::Colon)?;
            self.expect(TokKind::Newline)?;
            let b = self.parse_block()?;
            Some(b)
        } else {
            None
        };

        Ok(Stmt::If {
            arms,
            else_body,
            span: sp,
        })
    }

    fn parse_while(&mut self) -> Result<Stmt, ParseError> {
        let sp = self.expect_kw(Kw::Loop)?;
        let cond = self.parse_expr(0)?;
        self.expect(TokKind::Colon)?;
        self.expect(TokKind::Newline)?;
        let body = self.parse_block()?;
        Ok(Stmt::While {
            cond,
            body,
            span: sp,
        })
    }

    // -------- Pratt parser --------

    fn parse_expr(&mut self, min_bp: u8) -> Result<Expr, ParseError> {
        let mut lhs = self.parse_prefix()?;

        loop {
            // postfix: call
            if self.peek().kind == TokKind::LParen {
                let call_span = self.peek().span;
                self.next();
                let args = self.parse_args()?;
                self.expect(TokKind::RParen)?;
                lhs = Expr::Call {
                    callee: Box::new(lhs),
                    args,
                    span: call_span,
                };
                continue;
            } else if self.peek().kind == TokKind::Dot {
                let field_span = self.peek().span;
                self.next();
                let (fname, _) = self.take_ident()?;
                lhs = Expr::Field {
                    base: Box::new(lhs),
                    field: fname,
                    span: field_span,
                };
                continue;
            }

            let (op, l_bp, r_bp, non_assoc) = match infix_bp(&self.peek().kind) {
                Some(x) => x,
                None => break,
            };
            if l_bp < min_bp {
                break;
            }

            let op_span = self.peek().span;
            self.next(); // consume operator
            let rhs = self.parse_expr(r_bp)?;
            lhs = Expr::Binary {
                op,
                lhs: Box::new(lhs),
                rhs: Box::new(rhs),
                span: op_span,
            };

            if non_assoc && is_cmp(&self.peek().kind) {
                return Err(perr(self.peek().span, "comparison chaining is not allowed"));
            }
        }

        Ok(lhs)
    }

    fn parse_prefix(&mut self) -> Result<Expr, ParseError> {
        let t = self.peek();
        match &t.kind {
            TokKind::Minus => {
                let sp = t.span;
                self.next();
                let e = self.parse_expr(80)?;
                Ok(Expr::Unary {
                    op: UnOp::Neg,
                    expr: Box::new(e),
                    span: sp,
                })
            }
            TokKind::Not => {
                let sp = t.span;
                self.next();
                let e = self.parse_expr(80)?;
                Ok(Expr::Unary {
                    op: UnOp::Not,
                    expr: Box::new(e),
                    span: sp,
                })
            }
            TokKind::Int(v) => {
                let sp = t.span;
                let v = *v;
                self.next();
                Ok(Expr::Int(v, sp))
            }
            TokKind::Str(s) => {
                let sp = t.span;
                let s = s.clone();
                self.next();
                Ok(Expr::Str(s, sp))
            }
            TokKind::Ident(s) => {
                let sp = t.span;
                let s = s.clone();
                self.next();
                if self.peek().kind == TokKind::LBrace {
                    self.next();
                    let fields = self.parse_struct_fields()?;
                    self.expect(TokKind::RBrace)?;
                    Ok(Expr::StructLit {
                        name: s,
                        fields,
                        span: sp,
                    })
                } else {
                    Ok(Expr::Ident(s, sp))
                }
            }
            TokKind::Kw(Kw::Print) => {
                let sp = t.span;
                self.next();
                Ok(Expr::BuiltinPrint(sp))
            }
            TokKind::LParen => {
                self.next();
                let e = self.parse_expr(0)?;
                self.expect(TokKind::RParen)?;
                Ok(e)
            }
            _ => Err(perr(
                t.span,
                format!("expected expression, got {:?}", t.kind),
            )),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Arg>, ParseError> {
        let mut args = Vec::new();
        if self.peek().kind == TokKind::RParen {
            return Ok(args);
        }

        let mut saw_named = false;
        let mut saw_pos = false;

        loop {
            // named: ident ':' expr
            if let TokKind::Ident(name) = &self.peek().kind {
                if self.peek_n(1).kind == TokKind::Colon {
                    let name = name.clone();
                    let sp = self.peek().span;
                    self.next(); // ident
                    self.next(); // colon
                    let expr = self.parse_expr(0)?;
                    saw_named = true;
                    args.push(Arg::Named {
                        name,
                        expr,
                        span: sp,
                    });
                } else {
                    let expr = self.parse_expr(0)?;
                    saw_pos = true;
                    args.push(Arg::Pos(expr));
                }
            } else {
                let expr = self.parse_expr(0)?;
                saw_pos = true;
                args.push(Arg::Pos(expr));
            }

            if saw_named && saw_pos {
                return Err(perr(
                    self.peek().span,
                    "cannot mix named and positional args",
                ));
            }

            if self.peek().kind == TokKind::Comma {
                self.next();
                continue;
            }
            break;
        }
        Ok(args)
    }

    fn parse_struct_fields(&mut self) -> Result<Vec<(String, Expr, Span)>, ParseError> {
        let mut fields = Vec::new();
        self.skip_newlines();
        if self.peek().kind == TokKind::RBrace {
            return Ok(fields);
        }

        loop {
            let (name, sp) = self.take_ident()?;
            self.expect(TokKind::Colon)?;
            let expr = self.parse_expr(0)?;
            fields.push((name, expr, sp));

            if self.peek().kind == TokKind::Comma {
                self.next();
                self.skip_newlines();
                continue;
            }
            break;
        }
        Ok(fields)
    }
}

fn is_cmp(k: &TokKind) -> bool {
    matches!(
        k,
        TokKind::Lt | TokKind::Le | TokKind::Gt | TokKind::Ge | TokKind::EqEq | TokKind::Ne
    )
}

fn infix_bp(k: &TokKind) -> Option<(BinOp, u8, u8, bool)> {
    Some(match k {
        TokKind::Star => (BinOp::Mul, 70, 71, false),
        TokKind::Slash => (BinOp::Div, 70, 71, false),
        TokKind::Percent => (BinOp::Mod, 70, 71, false),

        TokKind::Plus => (BinOp::Add, 60, 61, false),
        TokKind::Minus => (BinOp::Sub, 60, 61, false),

        TokKind::Shl => (BinOp::Shl, 55, 56, false),
        TokKind::Shr => (BinOp::Shr, 55, 56, false),

        TokKind::Amp => (BinOp::BitAnd, 50, 51, false),
        TokKind::Caret => (BinOp::BitXor, 49, 50, false),
        TokKind::Pipe => (BinOp::BitOr, 48, 49, false),

        TokKind::Lt => (BinOp::Lt, 40, 41, true),
        TokKind::Le => (BinOp::Le, 40, 41, true),
        TokKind::Gt => (BinOp::Gt, 40, 41, true),
        TokKind::Ge => (BinOp::Ge, 40, 41, true),
        TokKind::EqEq => (BinOp::Eq, 40, 41, true),
        TokKind::Ne => (BinOp::Ne, 40, 41, true),

        TokKind::AndAnd => (BinOp::And, 30, 31, false),
        TokKind::OrOr => (BinOp::Or, 20, 21, false),

        _ => return None,
    })
}
