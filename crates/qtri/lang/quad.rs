use crate::lex::Kw;

pub fn keyword_of(s: &str) -> Option<Kw> {
    Some(match s {
        // Core keywords
        "from" => Kw::Use,
        "type" => Kw::Struct,
        "func" => Kw::Fn,
        "bind" => Kw::Let,
        "cell" => Kw::Var,
        "publ" => Kw::Pub,
        "priv" => Kw::Priv,
        "when" => Kw::If,
        "elif" => Kw::Elif,
        "else" => Kw::Else,
        "loop" => Kw::Loop,
        "back" => Kw::Return,
        "stop" => Kw::Break,
        "next" => Kw::Continue,
        "over" => Kw::In,

        // Stage1 completion (data structures / memory)
        "impl" => Kw::Impl,
        "self" => Kw::Self_, // method receiver
        "enum" => Kw::Enum,  // enum type
        "case" => Kw::Case,  // pattern match

        "true" => Kw::True,
        "fals" => Kw::False,

        _ => return None,
    })
}
