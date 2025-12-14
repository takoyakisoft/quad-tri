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
        "slf" => Kw::Self_,
        "enm" => Kw::Enum,
        "cas" => Kw::Case,

        "tru" => Kw::True,
        "fal" => Kw::False,

        _ => return None,
    })
}
