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
    let mut enums = Vec::new();
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    // Parse imports first
    while !p.at_eof() {
        if let TokKind::Kw(Kw::Use) = p.peek().kind {
            imports.push(p.parse_import()?);
            p.skip_newlines();
        } else {
            break;
        }
    }

    // Then definitions and implementations (type/impl/func)
    while !p.at_eof() {
        match p.peek().kind {
            TokKind::Kw(Kw::Enum) => {
                enums.push(p.parse_enum()?);
                p.skip_newlines();
            }
            TokKind::Kw(Kw::Struct) => {
                structs.push(p.parse_struct()?);
                p.skip_newlines();
            }
            TokKind::Kw(Kw::Impl) => {
                funcs.extend(p.parse_impl()?);
                p.skip_newlines();
            }
            TokKind::Kw(Kw::Fn) => {
                funcs.push(p.parse_func(None)?);
                p.skip_newlines();
            }
            TokKind::Eof => break,
            _ => {
                let t = p.peek();
                return Err(perr(
                    t.span,
                    format!("expected enum/type/impl/func, got {:?}", t.kind),
                ));
            }
        }
    }
    Ok(Program {
        imports,
        enums,
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

    fn is_type_name_start(kind: &TokKind) -> bool {
        matches!(kind, TokKind::Ident(_) | TokKind::LBrack)
    }

    fn can_parse_type_name_followed_by_coloncolon(&self) -> bool {
        let mut j = self.i;

        fn skip_type_name(tokens: &[Token], mut j: usize) -> Option<usize> {
            let kind = &tokens.get(j)?.kind;
            match kind {
                TokKind::LBrack => {
                    j += 1;
                    match &tokens.get(j)?.kind {
                        TokKind::Int(_) => j += 1,
                        _ => return None,
                    }
                    match &tokens.get(j)?.kind {
                        TokKind::RBrack => j += 1,
                        _ => return None,
                    }
                    skip_type_name(tokens, j)
                }
                TokKind::Ident(_) | TokKind::Kw(_) => {
                    j += 1;
                    if matches!(tokens.get(j).map(|t| &t.kind), Some(TokKind::Lt)) {
                        j += 1;
                        let mut depth = 1usize;
                        while j < tokens.len() {
                            match tokens[j].kind {
                                TokKind::Lt => depth += 1,
                                TokKind::Gt => {
                                    depth -= 1;
                                    if depth == 0 {
                                        j += 1;
                                        break;
                                    }
                                }
                                TokKind::Eof | TokKind::Newline => return None,
                                _ => {}
                            }
                            j += 1;
                        }
                        if depth != 0 {
                            return None;
                        }
                    }
                    Some(j)
                }
                _ => None,
            }
        }

        if !Self::is_type_name_start(&self.peek().kind) {
            return false;
        }

        j = match skip_type_name(self.tokens, j) {
            Some(n) => n,
            None => return false,
        };

        matches!(
            self.tokens.get(j).map(|t| &t.kind),
            Some(TokKind::ColonColon)
        )
    }

    fn take_variant_name(&mut self) -> Result<(String, Span), ParseError> {
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
                format!("expected variant name, got {:?}", t.kind),
            )),
        }
    }

    fn parse_type_name(&mut self) -> Result<(String, Span), ParseError> {
        match self.peek().kind {
            TokKind::LBrack => {
                let sp = self.expect(TokKind::LBrack)?;
                let len_tok = self.peek();
                let len = match len_tok.kind {
                    TokKind::Int(v) if v >= 0 => {
                        self.next();
                        if (v as u64) > (usize::MAX as u64) {
                            return Err(perr(len_tok.span, "array length too large for platform"));
                        }
                        v as usize
                    }
                    TokKind::Int(_) => {
                        return Err(perr(len_tok.span, "array length cannot be negative"));
                    }
                    _ => return Err(perr(len_tok.span, "expected array length")),
                };
                self.expect(TokKind::RBrack)?;
                let (elem, _) = self.parse_type_name()?;
                Ok((format!("[{len}]{elem}"), sp))
            }
            TokKind::Ident(_) => {
                let (base, sp) = self.take_ident()?;
                let mut name = base.clone();
                if self.peek().kind == TokKind::Lt {
                    self.next();
                    let mut args = Vec::new();
                    loop {
                        let (arg, _) = self.parse_type_name()?;
                        args.push(arg);
                        match self.peek().kind {
                            TokKind::Comma => {
                                self.next();
                                continue;
                            }
                            TokKind::Gt => {
                                self.next();
                                break;
                            }
                            _ => return Err(perr(self.peek().span, "expected ',' or '>' in type")),
                        }
                    }
                    name = format!("{base}<{}>", args.join(","));
                }
                Ok((name, sp))
            }
            _ => Err(perr(
                self.peek().span,
                format!("expected type name, got {:?}", self.peek().kind),
            )),
        }
    }

    fn parse_import(&mut self) -> Result<Import, ParseError> {
        self.expect_kw(Kw::Use)?;
        let (path, sp) = self.take_str()?;
        if self.peek().kind == TokKind::Newline {
            self.next();
        } else if !self.at_eof() {
            return Err(perr(self.peek().span, "expected newline or EOF"));
        }
        Ok(Import { path, span: sp })
    }

    fn parse_enum(&mut self) -> Result<EnumDef, ParseError> {
        let sp = self.expect_kw(Kw::Enum)?;
        let (name, _) = self.take_ident()?;
        self.expect(TokKind::Colon)?;
        self.expect(TokKind::Newline)?;
        self.expect(TokKind::Indent)?;
        let mut variants = Vec::new();
        loop {
            match &self.peek().kind {
                TokKind::Ident(variant) => {
                    let variant_name = variant.clone();
                    let v_span = self.peek().span;
                    self.next();
                    let mut fields = Vec::new();
                    if self.peek().kind == TokKind::LParen {
                        self.next();
                        if self.peek().kind != TokKind::RParen {
                            loop {
                                let (ty, sp) = self.parse_type_name()?;
                                fields.push((ty, sp));
                                match self.peek().kind {
                                    TokKind::Comma => {
                                        self.next();
                                    }
                                    TokKind::RParen => break,
                                    _ => {
                                        return Err(perr(
                                            self.peek().span,
                                            "expected ',' or ')' in enum variant",
                                        ));
                                    }
                                }
                            }
                        }
                        self.expect(TokKind::RParen)?;
                    }

                    variants.push(EnumVariant {
                        name: variant_name,
                        fields,
                        span: v_span,
                    });

                    match self.peek().kind {
                        TokKind::Newline => {
                            self.next();
                            continue;
                        }
                        TokKind::Dedent => continue,
                        _ => break,
                    }
                }
                TokKind::Dedent => {
                    self.next();
                    break;
                }
                TokKind::Newline => {
                    self.next();
                    // If dedent follows newline, handle it next iteration
                    if self.peek().kind == TokKind::Dedent {
                        self.next();
                        break;
                    }
                    continue;
                }
                other => {
                    return Err(perr(
                        self.peek().span,
                        format!("expected enum variant or dedent, got {:?}", other),
                    ));
                }
            }
        }
        Ok(EnumDef {
            name,
            variants,
            span: sp,
        })
    }

    fn parse_struct(&mut self) -> Result<StructDef, ParseError> {
        let sp = self.expect_kw(Kw::Struct)?;
        let (name, _) = self.take_ident()?;

        let mut fields = Vec::new();

        match self.peek().kind {
            TokKind::LBrace => {
                self.next();
                self.skip_newlines();

                if self.peek().kind != TokKind::RBrace {
                    loop {
                        let vis = match self.peek().kind {
                            TokKind::Kw(Kw::Pub) => {
                                self.next();
                                Visibility::Public
                            }
                            TokKind::Kw(Kw::Priv) => {
                                self.next();
                                Visibility::Private
                            }
                            _ => Visibility::Private,
                        };

                        let (fname, fsp) = self.take_ident()?;
                        self.expect(TokKind::Colon)?;
                        let (fty, _) = self.parse_type_name()?;
                        fields.push(Param {
                            name: fname,
                            ty: fty,
                            mutable: false,
                            vis,
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

                self.skip_newlines();
                self.expect(TokKind::RBrace)?;
                if self.peek().kind == TokKind::Newline {
                    self.next();
                } else if !self.at_eof() {
                    return Err(perr(self.peek().span, "expected newline or EOF"));
                }
            }
            TokKind::Colon => {
                self.next();
                self.expect(TokKind::Newline)?;
                self.expect(TokKind::Indent)?;
                self.skip_newlines();

                while self.peek().kind != TokKind::Dedent {
                    let vis = match self.peek().kind {
                        TokKind::Kw(Kw::Pub) => {
                            self.next();
                            Visibility::Public
                        }
                        TokKind::Kw(Kw::Priv) => {
                            self.next();
                            Visibility::Private
                        }
                        _ => Visibility::Private,
                    };

                    let (fname, fsp) = self.take_ident()?;
                    self.expect(TokKind::Colon)?;
                    let (fty, _) = self.parse_type_name()?;
                    fields.push(Param {
                        name: fname,
                        ty: fty,
                        mutable: false,
                        vis,
                        span: fsp,
                    });

                    if self.peek().kind != TokKind::Newline {
                        return Err(perr(self.peek().span, "expected newline after field"));
                    }
                    self.next();
                    self.skip_newlines();
                }

                self.expect(TokKind::Dedent)?;
            }
            _ => {
                return Err(perr(
                    self.peek().span,
                    "expected '{' or ':' after type name",
                ));
            }
        }

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
        let func_span = self.expect_kw(Kw::Fn)?;
        let (name, _) = self.take_ident()?;

        self.expect(TokKind::LParen)?;
        let params = self.parse_params(impl_target)?;
        self.expect(TokKind::RParen)?;

        let mut ret_ty: Option<String> = None;
        if self.peek().kind == TokKind::Arrow {
            self.next();
            let (ty, _) = self.parse_type_name()?;
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
                        vis: Visibility::Private,
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
            let (ty, _) = self.parse_type_name()?;
            out.push(Param {
                name,
                ty,
                mutable: false,
                vis: Visibility::Private,
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
        self.skip_newlines();
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
            TokKind::Kw(Kw::Let) | TokKind::Kw(Kw::Var) => self.parse_vardecl(),
            TokKind::Kw(Kw::If) => self.parse_if(),
            TokKind::Kw(Kw::Loop) => self.parse_while(),
            TokKind::Kw(Kw::Case) => self.parse_case(),
            TokKind::Kw(Kw::Break) => {
                let sp = t.span;
                self.next();
                self.expect(TokKind::Newline)?;
                Ok(Stmt::Break { span: sp })
            }
            TokKind::Kw(Kw::Continue) => {
                let sp = t.span;
                self.next();
                self.expect(TokKind::Newline)?;
                Ok(Stmt::Continue { span: sp })
            }
            TokKind::Kw(Kw::Return) => self.parse_back(),

            // assignment starts with Ident followed by := somewhere on the line (optionally via .field or [index])
            TokKind::Ident(_) if self.line_contains_assign() => self.parse_assign(),

            // otherwise: expression statement (println(...), foo(...), etc.)
            _ => {
                let sp = t.span;
                let expr = self.parse_expr(0)?;
                self.expect(TokKind::Newline)?;
                Ok(Stmt::Expr { expr, span: sp })
            }
        }
    }

    fn line_contains_assign(&self) -> bool {
        let mut idx = self.i;
        while idx < self.tokens.len() {
            match self.tokens[idx].kind {
                TokKind::Newline | TokKind::Dedent => return false,
                TokKind::Assign => return true,
                _ => idx += 1,
            }
        }
        false
    }

    fn parse_vardecl(&mut self) -> Result<Stmt, ParseError> {
        let t = self.peek();
        let (mutable, sp) = match &t.kind {
            TokKind::Kw(Kw::Var) => (true, t.span),
            TokKind::Kw(Kw::Let) => (false, t.span),
            _ => return Err(perr(t.span, "expected cell/bind")),
        };
        self.next();

        let (name, _) = self.take_ident()?;
        self.expect(TokKind::Colon)?;
        let (ty, _) = self.parse_type_name()?;
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
        let target = match self.peek().kind {
            TokKind::Dot => {
                self.next();
                let (field, _) = self.take_ident()?;
                AssignTarget::Field { base: name, field }
            }
            TokKind::LBrack => {
                self.next();
                let index = self.parse_expr(0)?;
                self.expect(TokKind::RBrack)?;
                AssignTarget::Index { base: name, index }
            }
            _ => AssignTarget::Name(name),
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
        let sp = self.expect_kw(Kw::If)?;
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

    fn parse_case(&mut self) -> Result<Stmt, ParseError> {
        let sp = self.expect_kw(Kw::Case)?;
        let scrutinee = self.parse_expr(0)?;
        self.expect(TokKind::Colon)?;
        self.expect(TokKind::Newline)?;
        self.expect(TokKind::Indent)?;

        let mut arms = Vec::new();
        loop {
            match self.peek().kind {
                TokKind::Kw(Kw::If) => {
                    self.next();
                    let pat = self.parse_pattern()?;
                    self.expect(TokKind::Colon)?;
                    let body = self.parse_block()?;
                    arms.push((pat, body));
                }
                TokKind::Dedent => {
                    self.next();
                    break;
                }
                _ => {
                    return Err(perr(
                        self.peek().span,
                        "expected 'when' or dedent inside case block",
                    ));
                }
            }
        }

        Ok(Stmt::Case {
            scrutinee,
            arms,
            span: sp,
        })
    }

    fn parse_pattern(&mut self) -> Result<Pattern, ParseError> {
        if !self.can_parse_type_name_followed_by_coloncolon() {
            return Err(perr(
                self.peek().span,
                "expected enum pattern <Type>::<Variant>",
            ));
        }
        let (enum_name, _) = self.parse_type_name()?;
        self.expect(TokKind::ColonColon)?;
        let (variant, sp) = self.take_variant_name()?;
        let mut bindings = Vec::new();
        if self.peek().kind == TokKind::LParen {
            self.next();
            if self.peek().kind != TokKind::RParen {
                loop {
                    let (name, _) = self.take_ident()?;
                    bindings.push(name);
                    match self.peek().kind {
                        TokKind::Comma => {
                            self.next();
                        }
                        TokKind::RParen => break,
                        _ => {
                            return Err(perr(
                                self.peek().span,
                                "expected ',' or ')' in pattern bindings",
                            ));
                        }
                    }
                }
            }
            self.expect(TokKind::RParen)?;
        }

        Ok(Pattern::EnumVariant {
            enum_name,
            variant,
            bindings,
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
            } else if self.peek().kind == TokKind::LBrack {
                let idx_span = self.peek().span;
                self.next();
                let index = self.parse_expr(0)?;
                self.expect(TokKind::RBrack)?;
                lhs = Expr::Index {
                    base: Box::new(lhs),
                    index: Box::new(index),
                    span: idx_span,
                };
                continue;
            } else if self.peek().kind == TokKind::Question {
                let sp = self.peek().span;
                self.next();
                lhs = Expr::Try {
                    expr: Box::new(lhs),
                    span: sp,
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

        if self.can_parse_type_name_followed_by_coloncolon() {
            let (enum_name, sp) = self.parse_type_name()?;
            self.expect(TokKind::ColonColon)?;
            let (variant, _) = self.take_variant_name()?;
            let mut args = Vec::new();
            if self.peek().kind == TokKind::LParen {
                self.next();
                if self.peek().kind != TokKind::RParen {
                    loop {
                        args.push(self.parse_expr(0)?);
                        match self.peek().kind {
                            TokKind::Comma => {
                                self.next();
                                self.skip_newlines();
                            }
                            TokKind::RParen => break,
                            _ => return Err(perr(self.peek().span, "expected ',' or ')'")),
                        }
                    }
                }
                self.expect(TokKind::RParen)?;
            }
            return Ok(Expr::EnumLit {
                enum_name,
                variant,
                args,
                span: sp,
            });
        }

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
            TokKind::Kw(Kw::Box | Kw::Drop) => {
                let sp = t.span;
                let name = match t.kind {
                    TokKind::Kw(Kw::Box) => "heap",
                    TokKind::Kw(Kw::Drop) => "free",
                    _ => unreachable!(),
                };
                self.next();
                Ok(Expr::Ident(name.into(), sp))
            }
            TokKind::LBrack => {
                let sp = t.span;
                self.next();
                self.skip_newlines();

                let mut had_indent = false;
                if self.peek().kind == TokKind::Indent {
                    had_indent = true;
                    self.next();
                    self.skip_newlines();
                }

                let mut elements = Vec::new();
                if self.peek().kind != TokKind::RBrack {
                    loop {
                        let expr = self.parse_expr(0)?;
                        elements.push(expr);
                        self.skip_newlines();
                        match self.peek().kind {
                            TokKind::Comma => {
                                self.next();
                                self.skip_newlines();
                                continue;
                            }
                            TokKind::RBrack => break,
                            TokKind::Dedent if had_indent => {
                                self.next();
                                self.skip_newlines();
                                if self.peek().kind == TokKind::RBrack {
                                    break;
                                }
                                return Err(perr(
                                    self.peek().span,
                                    "expected ']' after dedent in array literal",
                                ));
                            }
                            _ => {
                                return Err(perr(
                                    self.peek().span,
                                    "expected ',' or ']' in array literal",
                                ));
                            }
                        }
                    }
                }

                if had_indent {
                    self.skip_newlines();
                    if self.peek().kind == TokKind::Dedent {
                        self.next();
                        self.skip_newlines();
                    }
                }
                self.expect(TokKind::RBrack)?;
                Ok(Expr::ArrayLit { elements, span: sp })
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
                } else if self.peek().kind == TokKind::ColonColon {
                    self.next(); // consume ::
                    let (variant, _) = self.take_ident()?;
                    let mut args = Vec::new();
                    if self.peek().kind == TokKind::LParen {
                        self.next();
                        if self.peek().kind != TokKind::RParen {
                            loop {
                                args.push(self.parse_expr(0)?);
                                match self.peek().kind {
                                    TokKind::Comma => {
                                        self.next();
                                        self.skip_newlines(); // allow newline after comma
                                    }
                                    TokKind::RParen => break,
                                    _ => return Err(perr(self.peek().span, "expected ',' or ')'")),
                                }
                            }
                        }
                        self.expect(TokKind::RParen)?;
                    }
                    Ok(Expr::EnumLit {
                        enum_name: s,
                        variant,
                        args,
                        span: sp,
                    })
                } else {
                    Ok(Expr::Ident(s, sp))
                }
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
        if self.peek().kind == TokKind::Indent {
            self.next();
            self.skip_newlines();
        }
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
        self.skip_newlines();
        if self.peek().kind == TokKind::Dedent {
            self.next();
            self.skip_newlines();
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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lex::{self, Language};

    #[test]
    fn parses_quad_heap_and_stack_struct_example() {
        let src = "type User:\n    publ name: text\n    publ age: int\n\nfunc main() -> int:\n    # ---------------------------------------------------\n    # 1. Stack allocation\n    #    Rust: let u = User { name: \"Bob\", age: 20 };\n    #    Go:   u := User{Name: \"Bob\", Age: 20}\n    # ---------------------------------------------------\n    cell u: User := User {\n        name: \"Bob\",\n        age: 20\n    }\n    \n    # Field access uses the dot operator\n    println(u.name)\n\n\n    # ---------------------------------------------------\n    # 2. Heap allocation\n    #    Rust: let p = Box::new(User { ... });\n    #    Go:   p := &User{ ... }\n    # ---------------------------------------------------\n    # Passing the struct value to heap() moves it to the heap and returns ref<User>.\n    \n    cell p: ref<User> := heap(User {\n        name: \"Alice\",\n        age: 30\n    })\n\n    # Pointer access supports automatic dereference\n    println(p.name)\n\n    # Heap values must be freed\n    free(p)\n\n    back 0\n";

        let tokens = lex::lex_str(Language::Quad, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_tri_heap_and_stack_struct_example() {
        let src = "typ Usr:\n    pub nam: text\n    pub age: int\n\ndef main() -> int:\n    # ---------------------------------------------------\n    # 1. Stack allocation\n    #    Construct directly without new/make\n    # ---------------------------------------------------\n    var u: Usr := Usr {\n        nam: \"Bob\",\n        age: 20\n    }\n    \n    println(u.nam)\n\n\n    # ---------------------------------------------------\n    # 2. Heap allocation\n    #    Wrap with mem()\n    # ---------------------------------------------------\n    var p: ptr<Usr> := mem(Usr {\n        nam: \"Alice\",\n        age: 30\n    })\n\n    println(p.nam)\n\n    # Free the heap allocation\n    del(p)\n\n    ret 0\n";

        let tokens = lex::lex_str(Language::Tri, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_quad_dict_example() {
        let src = "func main() -> int:\n    # Create a dictionary: dict(key type, value type)\n    cell scores: dict<text, int> := dict(text, int)\n\n    # Insert or update via index access\n    # Go-style [] access is convenient compared to Rust insert\n    scores[\"Alice\"] := 100\n    scores[\"Bob\"]   := 80\n\n    # Read value\n    println(scores[\"Alice\"])\n    \n    back 0\n";

        let tokens = lex::lex_str(Language::Quad, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_tri_map_example() {
        let src = "def main() -> int:\n    # Create a map: map(key type, value type)\n    var scs: map<text, int> := map(text, int)\n\n    # Insert\n    scs[\"Alice\"] := 100\n\n    # Read\n    println(scs[\"Alice\"])\n    \n    ret 0\n";

        let tokens = lex::lex_str(Language::Tri, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_quad_text_operations_example() {
        let src = "func main() -> int:\n    cell s: text := \"Hello\"\n\n    # 1. Concatenation (+) allocates a fresh string\n    cell msg: text := s + \" World\"\n    \n    # 2. Method calls (modern) support msg.size()\n    when msg.size() > 10:\n        println(\"Too long\")\n\n    # 3. Substring returns a slice into the original buffer\n    cell sub: text := msg.sub(0, 5)  # \"Hello\"\n    \n    # 4. Search\n    when msg.has(\"World\"):\n        println(\"Found!\")\n\n    # 5. Format with placeholders\n    cell log: text := fmt(\"User: {}, ID: {}\", \"Bob\", 123)\n    println(log)\n\n    free(msg)\n    free(log)\n    \n    back 0\n";

        let tokens = lex::lex_str(Language::Quad, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_tri_text_operations_example() {
        let src = "def main() -> int:\n    var s: text := \"Hello\"\n\n    # Concatenation\n    var msg: text := s + \" World\"\n\n    # Method call len()\n    iff msg.len() > 10:\n        println(\"Big\")\n\n    # Substring\n    var sub: text := msg.sub(0, 5)\n    \n    # Search\n    iff msg.has(\"World\"):\n        println(\"Yes\")\n\n    # Format\n    var log: text := fmt(\"User: {}, ID: {}\", \"Bob\", 123)\n    println(log)\n\n    del(msg)\n    del(log)\n    ret 0\n";

        let tokens = lex::lex_str(Language::Tri, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_quad_enum_case_example() {
        let src = "enum Event:\n    Quit\n    Click(int, int)\n    Key(text)\n\nfunc handle(e: Event) -> void:\n    case e:\n        when Event::Quit:\n            println(\"Bye\")\n\n        when Event::Click(x, y):\n            println(\"Clicked at:\")\n            println(x)\n\n        when Event::Key(k):\n            println(\"Key pressed:\")\n            println(k)\n";

        let tokens = lex::lex_str(Language::Quad, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_tri_enum_case_example() {
        let src = "enm Evt:\n    Qit\n    Clk(int, int)\n    Key(text)\n\n\ndef hnd(e: Evt) -> void:\n    cas e:\n        iff Evt::Qit:\n            println(\"Bye\")\n\n        iff Evt::Clk(x, y):\n            println(\"Clk\")\n            println(x)\n\n        iff Evt::Key(k):\n            println(\"Key\")\n            println(k)\n";

        let tokens = lex::lex_str(Language::Tri, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_quad_fixed_array_examples() {
        let src = "func main() -> int:\n    # 1. Simple fixed array on the stack\n    cell arr: [5]int := [10, 20, 30, 40, 50]\n    \n    println(\"Element 0:\")\n    println(arr[0])\n\n    # 2. Nested arrays (2 x 3 matrix)\n    cell matrix: [2][3]int := [\n        [1, 2, 3],\n        [4, 5, 6]\n    ]\n\n    println(\"Matrix[1][2]:\")\n    println(matrix[1][2])\n\n    # 3. Replace a full row\n    matrix[0] := [9, 8, 7]\n    \n    println(\"Matrix[0][0] changed:\")\n    println(matrix[0][0])\n\n    # 4. Explicit zeroed buffer\n    cell buffer: [4]int := [0, 0, 0, 0]\n\n    back 0\n";

        let tokens = lex::lex_str(Language::Quad, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }

    #[test]
    fn parses_tri_fixed_array_examples() {
        let src = "def main() -> int:\n    # 1. Fixed-length array\n    var arr: [3]int := [100, 200, 300]\n    \n    println(arr[0])\n\n    # 2. Two-dimensional array (2 x 2)\n    var mtx: [2][2]int := [\n        [1, 0],\n        [0, 1]\n    ]\n\n    println(\"Identity Matrix:\")\n    println(mtx[0][0])\n    println(mtx[1][1])\n\n    # 3. Fixed array of strings\n    var names: [3]text := [\"Alice\", \"Bob\", \"Eve\"]\n    \n    println(names[1])\n\n    ret 0\n";

        let tokens = lex::lex_str(Language::Tri, src).expect("lexing succeeds");
        let _ = parse_program(&tokens).expect("program parses");
    }
}
