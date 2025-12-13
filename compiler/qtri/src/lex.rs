use std::path::Path;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Language {
    Quad,
    Tri,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Kw {
    From,
    Type,
    Func,
    Impl,
    Make,
    Self_,
    List,
    Push,
    Size,
    Enum,
    Case,
    Lock,
    Vars,
    When,
    Elif,
    Else,
    Loop,
    Back,
    Open,
    Read,
    Save,
    Shut,
    Heap,
    Free,
    Okay,
    Fail,
    Trap,
    Print,
    Stop,
    Next,
    Over,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TokKind {
    // layout
    Newline,
    Indent,
    Dedent,
    Eof,

    // literals
    Ident(String),
    Int(i64),
    Str(String),

    // keywords (language-normalized)
    Kw(Kw),

    // punctuation
    Colon,
    Comma,
    Dot,
    LBrace,
    RBrace,
    LParen,
    RParen,
    LBrack,
    RBrack,

    // operators
    Arrow,  // ->
    Assign, // :=
    EqEq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    AndAnd,
    OrOr,
    Not,
    Plus,
    Minus,
    Star,
    Slash,
    Percent,
    Amp,
    Caret,
    Pipe,
    Shl,
    Shr,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Span {
    pub line: usize, // 1-based
    pub col: usize,  // 1-based (byte index in line + 1)
}

#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub kind: TokKind,
    pub span: Span,
}

#[derive(Debug)]
pub struct LexError {
    pub span: Span,
    pub msg: String,
}

impl std::fmt::Display for LexError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}: {}", self.span.line, self.span.col, self.msg)
    }
}
impl std::error::Error for LexError {}

fn err(line: usize, col: usize, msg: impl Into<String>) -> LexError {
    LexError {
        span: Span { line, col },
        msg: msg.into(),
    }
}

pub fn lex_file(lang: Language, path: &Path) -> Result<Vec<Token>, LexError> {
    let src = std::fs::read_to_string(path)
        .map_err(|e| err(1, 1, format!("failed to read file: {e}")))?;
    lex_str(lang, &src)
}

pub fn lex_str(lang: Language, src: &str) -> Result<Vec<Token>, LexError> {
    let kw_fn: fn(&str) -> Option<Kw> = match lang {
        Language::Quad => crate::quad::keyword_of,
        Language::Tri => crate::tri::keyword_of,
    };

    let mut out: Vec<Token> = Vec::new();
    let mut indents: Vec<usize> = vec![0];

    let total_lines = src.split('\n').count().max(1);

    for (li0, raw_line) in src.split('\n').enumerate() {
        let line_no = li0 + 1;
        let mut line = raw_line;
        if line.ends_with('\r') {
            line = &line[..line.len() - 1];
        }

        // count leading spaces / reject tabs
        let mut idx = 0usize;
        let bytes = line.as_bytes();
        while idx < bytes.len() {
            match bytes[idx] {
                b' ' => idx += 1,
                b'\t' => return Err(err(line_no, idx + 1, "tabs are forbidden")),
                _ => break,
            }
        }
        let indent = idx;

        // strip comment-only / blank lines (do not affect indentation)
        let rest = &line[idx..];
        let rest_trim = rest.trim_start();
        if rest_trim.is_empty() || rest_trim.starts_with('#') {
            out.push(Token {
                kind: TokKind::Newline,
                span: Span {
                    line: line_no,
                    col: 1,
                },
            });
            continue;
        }

        // indentation change
        let cur = *indents.last().unwrap();
        if indent > cur {
            indents.push(indent);
            out.push(Token {
                kind: TokKind::Indent,
                span: Span {
                    line: line_no,
                    col: 1,
                },
            });
        } else if indent < cur {
            while *indents.last().unwrap() > indent {
                indents.pop();
                out.push(Token {
                    kind: TokKind::Dedent,
                    span: Span {
                        line: line_no,
                        col: 1,
                    },
                });
            }
            if *indents.last().unwrap() != indent {
                return Err(err(line_no, 1, "indentation level mismatch"));
            }
        }

        // scan the rest of the line
        let mut i = idx;
        while i < bytes.len() {
            // skip spaces
            if bytes[i] == b' ' {
                i += 1;
                continue;
            }
            // comment (after code)
            if bytes[i] == b'#' {
                break;
            }

            let col = i + 1;

            // string literal: "..."
            if bytes[i] == b'"' {
                i += 1;
                let mut s = String::new();
                while i < bytes.len() {
                    let c = bytes[i];
                    if c == b'"' {
                        i += 1;
                        break;
                    }
                    if c == b'\\' {
                        i += 1;
                        if i >= bytes.len() {
                            return Err(err(line_no, col, "unterminated string escape"));
                        }
                        let esc = bytes[i];
                        match esc {
                            b'n' => s.push('\n'),
                            b't' => s.push('\t'),
                            b'r' => s.push('\r'),
                            b'\\' => s.push('\\'),
                            b'"' => s.push('"'),
                            _ => return Err(err(line_no, i + 1, "unknown escape")),
                        }
                        i += 1;
                        continue;
                    }
                    s.push(c as char);
                    i += 1;
                }
                out.push(Token {
                    kind: TokKind::Str(s),
                    span: Span { line: line_no, col },
                });
                continue;
            }

            // number
            if bytes[i].is_ascii_digit() {
                let start = i;
                while i < bytes.len() && bytes[i].is_ascii_digit() {
                    i += 1;
                }
                let text = &line[start..i];
                let v: i64 = text.parse().map_err(|_| err(line_no, col, "invalid int"))?;
                out.push(Token {
                    kind: TokKind::Int(v),
                    span: Span { line: line_no, col },
                });
                continue;
            }

            // identifier / keyword
            if bytes[i].is_ascii_alphabetic() || bytes[i] == b'_' {
                let start = i;
                i += 1;
                while i < bytes.len() {
                    let c = bytes[i];
                    if c.is_ascii_alphanumeric() || c == b'_' {
                        i += 1;
                    } else {
                        break;
                    }
                }
                let text = &line[start..i];
                if let Some(k) = kw_fn(text) {
                    out.push(Token {
                        kind: TokKind::Kw(k),
                        span: Span { line: line_no, col },
                    });
                } else {
                    out.push(Token {
                        kind: TokKind::Ident(text.to_string()),
                        span: Span { line: line_no, col },
                    });
                }
                continue;
            }

            // two-char operators
            let two = if i + 1 < bytes.len() {
                &line[i..i + 2]
            } else {
                ""
            };
            if !two.is_empty() {
                let kind = match two {
                    "->" => Some(TokKind::Arrow),
                    ":=" => Some(TokKind::Assign),
                    "==" => Some(TokKind::EqEq),
                    "!=" => Some(TokKind::Ne),
                    "<=" => Some(TokKind::Le),
                    ">=" => Some(TokKind::Ge),
                    "&&" => Some(TokKind::AndAnd),
                    "||" => Some(TokKind::OrOr),
                    "<<" => Some(TokKind::Shl),
                    ">>" => Some(TokKind::Shr),
                    _ => None,
                };
                if let Some(k) = kind {
                    out.push(Token {
                        kind: k,
                        span: Span { line: line_no, col },
                    });
                    i += 2;
                    continue;
                }
            }

            // single-char
            let ch = bytes[i] as char;
            let kind = match ch {
                ':' => TokKind::Colon,
                ',' => TokKind::Comma,
                '.' => TokKind::Dot,
                '{' => TokKind::LBrace,
                '}' => TokKind::RBrace,
                '(' => TokKind::LParen,
                ')' => TokKind::RParen,
                '[' => TokKind::LBrack,
                ']' => TokKind::RBrack,

                '<' => TokKind::Lt,
                '>' => TokKind::Gt,
                '!' => TokKind::Not,

                '+' => TokKind::Plus,
                '-' => TokKind::Minus,
                '*' => TokKind::Star,
                '/' => TokKind::Slash,
                '%' => TokKind::Percent,

                '&' => TokKind::Amp,
                '^' => TokKind::Caret,
                '|' => TokKind::Pipe,
                _ => return Err(err(line_no, col, format!("unexpected char: {ch:?}"))),
            };
            out.push(Token {
                kind,
                span: Span { line: line_no, col },
            });
            i += 1;
        }

        out.push(Token {
            kind: TokKind::Newline,
            span: Span {
                line: line_no,
                col: bytes.len().max(1),
            },
        });
    }

    // finalize: close open indents
    while indents.len() > 1 {
        indents.pop();
        out.push(Token {
            kind: TokKind::Dedent,
            span: Span {
                line: total_lines,
                col: 1,
            },
        });
    }
    out.push(Token {
        kind: TokKind::Eof,
        span: Span {
            line: total_lines,
            col: 1,
        },
    });

    Ok(out)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn kinds(lang: Language, src: &str) -> Vec<TokKind> {
        lex_str(lang, src)
            .unwrap()
            .into_iter()
            .map(|t| t.kind)
            .collect()
    }

    #[test]
    fn lex_quad_minimal_with_arrow_and_indent() {
        let src = "func main() -> intg:\n    echo(123)\n    back 0";
        let ks = kinds(Language::Quad, src);

        // 期待する並び（Span/行番号は見ない）
        let expected = vec![
            TokKind::Kw(Kw::Func),
            TokKind::Ident("main".into()),
            TokKind::LParen,
            TokKind::RParen,
            TokKind::Arrow,
            TokKind::Ident("intg".into()),
            TokKind::Colon,
            TokKind::Newline,
            TokKind::Indent,
            TokKind::Kw(Kw::Print),
            TokKind::LParen,
            TokKind::Int(123),
            TokKind::RParen,
            TokKind::Newline,
            TokKind::Kw(Kw::Back),
            TokKind::Int(0),
            TokKind::Newline,
            TokKind::Dedent,
            TokKind::Eof,
        ];

        assert_eq!(ks, expected);
    }

    #[test]
    fn lex_tri_keywords_map_to_normalized_kw() {
        let src = "def main() -> int:\n    prn(123)\n    ret 0";
        let ks = kinds(Language::Tri, src);

        // Tri側も内部表現は Kw(Func/Echo/Back) に正規化される
        let expected = vec![
            TokKind::Kw(Kw::Func),
            TokKind::Ident("main".into()),
            TokKind::LParen,
            TokKind::RParen,
            TokKind::Arrow,
            TokKind::Ident("int".into()),
            TokKind::Colon,
            TokKind::Newline,
            TokKind::Indent,
            TokKind::Kw(Kw::Print),
            TokKind::LParen,
            TokKind::Int(123),
            TokKind::RParen,
            TokKind::Newline,
            TokKind::Kw(Kw::Back),
            TokKind::Int(0),
            TokKind::Newline,
            TokKind::Dedent,
            TokKind::Eof,
        ];

        assert_eq!(ks, expected);
    }
}
