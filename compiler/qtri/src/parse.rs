use crate::ast::*;
use crate::lex::{Kw, Span, TokKind, Token};

#[derive(Debug)]
pub struct ParseError { pub span: Span, pub msg: String }
impl std::fmt::Display for ParseError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.line, self.span.col, self.msg)
    }
}
impl std::error::Error for ParseError {}
fn perr(span: Span, msg: impl Into<String>) -> ParseError { ParseError { span, msg: msg.into() } }

pub fn parse_program(tokens: &[Token]) -> Result<Program, ParseError> {
    let mut p = Parser { tokens, i: 0 };
    p.skip_newlines();
    let mut funcs = Vec::new();
    while !p.at_eof() {
        funcs.push(p.parse_func()?);
        p.skip_newlines();
    }
    Ok(Program { funcs })
}

struct Parser<'a> { tokens: &'a [Token], i: usize }
impl<'a> Parser<'a> {
    fn at_eof(&self) -> bool { matches!(self.peek().kind, TokKind::Eof) }
    fn peek(&self) -> &'a Token { self.tokens.get(self.i).unwrap_or_else(|| self.tokens.last().unwrap()) }
    fn peek_n(&self, n: usize) -> &'a Token {
        self.tokens.get(self.i + n).unwrap_or_else(|| self.tokens.last().unwrap())
    }
    fn next(&mut self) -> &'a Token { let t = self.peek(); self.i = (self.i + 1).min(self.tokens.len()); t }

    fn skip_newlines(&mut self) {
        while matches!(self.peek().kind, TokKind::Newline) { self.next(); }
    }

    fn expect(&mut self, kind: TokKind) -> Result<Span, ParseError> {
        let t = self.peek();
        if t.kind == kind { let s = t.span; self.next(); Ok(s) }
        else { Err(perr(t.span, format!("expected {:?}, got {:?}", kind, t.kind))) }
    }
    fn expect_kw(&mut self, kw: Kw) -> Result<Span, ParseError> {
        let t = self.peek();
        match &t.kind {
            TokKind::Kw(k) if *k == kw => { let s = t.span; self.next(); Ok(s) }
            _ => Err(perr(t.span, format!("expected keyword {:?}, got {:?}", kw, t.kind))),
        }
    }
    fn take_ident(&mut self) -> Result<(String, Span), ParseError> {
        let t = self.peek();
        match &t.kind {
            TokKind::Ident(s) => { let sp = t.span; let name = s.clone(); self.next(); Ok((name, sp)) }
            _ => Err(perr(t.span, format!("expected identifier, got {:?}", t.kind))),
        }
    }

    fn parse_func(&mut self) -> Result<Func, ParseError> {
        let func_span = self.expect_kw(Kw::Func)?;
        let (name, _) = self.take_ident()?;

        self.expect(TokKind::LParen)?;
        let params = self.parse_params()?;
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
        Ok(Func { name, params, ret_ty, body, span: func_span })
    }

    fn parse_params(&mut self) -> Result<Vec<Param>, ParseError> {
        let mut out = Vec::new();
        if self.peek().kind == TokKind::RParen { return Ok(out); }
        loop {
            let (name, sp) = self.take_ident()?;
            self.expect(TokKind::Colon)?;
            let (ty, _) = self.take_ident()?;
            out.push(Param { name, ty, span: sp });

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
            TokKind::Kw(Kw::Stop) => { let sp = t.span; self.next(); self.expect(TokKind::Newline)?; Ok(Stmt::Break{span:sp}) }
            TokKind::Kw(Kw::Next) => { let sp = t.span; self.next(); self.expect(TokKind::Newline)?; Ok(Stmt::Continue{span:sp}) }
            TokKind::Kw(Kw::Back) => self.parse_back(),

            // assignment starts with Ident then :=
            TokKind::Ident(_) if self.peek_n(1).kind == TokKind::Assign => self.parse_assign(),

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
        Ok(Stmt::VarDecl { mutable, name, ty, init, span: sp })
    }

    fn parse_assign(&mut self) -> Result<Stmt, ParseError> {
        let (name, sp) = self.take_ident()?;
        self.expect(TokKind::Assign)?;
        let expr = self.parse_expr(0)?;
        self.expect(TokKind::Newline)?;
        Ok(Stmt::Assign { name, expr, span: sp })
    }

    fn parse_back(&mut self) -> Result<Stmt, ParseError> {
        let t = self.peek();
        let sp = t.span;
        self.next();
        let expr = if self.peek().kind == TokKind::Newline { None } else { Some(self.parse_expr(0)?) };
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

        Ok(Stmt::If { arms, else_body, span: sp })
    }

    fn parse_while(&mut self) -> Result<Stmt, ParseError> {
        let sp = self.expect_kw(Kw::Loop)?;
        let cond = self.parse_expr(0)?;
        self.expect(TokKind::Colon)?;
        self.expect(TokKind::Newline)?;
        let body = self.parse_block()?;
        Ok(Stmt::While { cond, body, span: sp })
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
                lhs = Expr::Call { callee: Box::new(lhs), args, span: call_span };
                continue;
            }

            let (op, l_bp, r_bp, non_assoc) = match infix_bp(&self.peek().kind) {
                Some(x) => x,
                None => break,
            };
            if l_bp < min_bp { break; }

            let op_span = self.peek().span;
            self.next(); // consume operator
            let rhs = self.parse_expr(r_bp)?;
            lhs = Expr::Binary { op, lhs: Box::new(lhs), rhs: Box::new(rhs), span: op_span };

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
                let sp = t.span; self.next();
                let e = self.parse_expr(80)?;
                Ok(Expr::Unary { op: UnOp::Neg, expr: Box::new(e), span: sp })
            }
            TokKind::Not => {
                let sp = t.span; self.next();
                let e = self.parse_expr(80)?;
                Ok(Expr::Unary { op: UnOp::Not, expr: Box::new(e), span: sp })
            }
            TokKind::Int(v) => { let sp = t.span; let v = *v; self.next(); Ok(Expr::Int(v, sp)) }
            TokKind::Str(s) => { let sp = t.span; let s = s.clone(); self.next(); Ok(Expr::Str(s, sp)) }
            TokKind::Ident(s) => { let sp = t.span; let s = s.clone(); self.next(); Ok(Expr::Ident(s, sp)) }
            TokKind::Kw(Kw::Print) => { let sp = t.span; self.next(); Ok(Expr::BuiltinPrint(sp)) }
            TokKind::LParen => {
                self.next();
                let e = self.parse_expr(0)?;
                self.expect(TokKind::RParen)?;
                Ok(e)
            }
            _ => Err(perr(t.span, format!("expected expression, got {:?}", t.kind))),
        }
    }

    fn parse_args(&mut self) -> Result<Vec<Arg>, ParseError> {
        let mut args = Vec::new();
        if self.peek().kind == TokKind::RParen { return Ok(args); }

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
                    args.push(Arg::Named { name, expr, span: sp });
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
                return Err(perr(self.peek().span, "cannot mix named and positional args"));
            }

            if self.peek().kind == TokKind::Comma {
                self.next();
                continue;
            }
            break;
        }
        Ok(args)
    }
}

fn is_cmp(k: &TokKind) -> bool {
    matches!(k, TokKind::Lt|TokKind::Le|TokKind::Gt|TokKind::Ge|TokKind::EqEq|TokKind::Ne)
}

fn infix_bp(k: &TokKind) -> Option<(BinOp, u8, u8, bool)> {
    Some(match k {
        TokKind::Star    => (BinOp::Mul, 70, 71, false),
        TokKind::Slash   => (BinOp::Div, 70, 71, false),
        TokKind::Percent => (BinOp::Mod, 70, 71, false),

        TokKind::Plus  => (BinOp::Add, 60, 61, false),
        TokKind::Minus => (BinOp::Sub, 60, 61, false),

        TokKind::Shl => (BinOp::Shl, 55, 56, false),
        TokKind::Shr => (BinOp::Shr, 55, 56, false),

        TokKind::Amp   => (BinOp::BitAnd, 50, 51, false),
        TokKind::Caret => (BinOp::BitXor, 49, 50, false),
        TokKind::Pipe  => (BinOp::BitOr, 48, 49, false),

        TokKind::Lt   => (BinOp::Lt, 40, 41, true),
        TokKind::Le   => (BinOp::Le, 40, 41, true),
        TokKind::Gt   => (BinOp::Gt, 40, 41, true),
        TokKind::Ge   => (BinOp::Ge, 40, 41, true),
        TokKind::EqEq => (BinOp::Eq, 40, 41, true),
        TokKind::Ne   => (BinOp::Ne, 40, 41, true),

        TokKind::AndAnd => (BinOp::And, 30, 31, false),
        TokKind::OrOr   => (BinOp::Or, 20, 21, false),

        _ => return None,
    })
}
