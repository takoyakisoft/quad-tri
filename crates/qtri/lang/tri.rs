use crate::lex::Kw;

pub fn keyword_of(s: &str) -> Option<Kw> {
    Some(match s {
        // Core keywords
        "use" => Kw::Use, // import equivalent (normalized to From internally)
        "typ" => Kw::Struct,
        "def" => Kw::Fn,
        "let" => Kw::Let,
        "var" => Kw::Var,
        "pub" => Kw::Pub,
        "prv" => Kw::Priv,
        "iff" => Kw::If,
        "elf" => Kw::Elif,
        "els" => Kw::Else,
        "for" => Kw::Loop,
        "ret" => Kw::Return,
        "brk" => Kw::Break,
        "nxt" => Kw::Continue,
        "ovr" => Kw::In,

        // Stage1 completion (data structures / memory)
        "imp" => Kw::Impl,
        "new" => Kw::Make,
        "slf" => Kw::Self_,
        "vec" => Kw::List,
        "psh" => Kw::Push,
        "enm" => Kw::Enum,
        "cas" => Kw::Case,

        // Stage1 completion (I/O / errors)
        "opn" => Kw::Open,
        "red" => Kw::Read,
        "wrt" => Kw::Write,
        "cls" => Kw::Close,
        "mem" => Kw::Box,
        "del" => Kw::Drop,
        "die" => Kw::Panic,

        _ => return None,
    })
}
