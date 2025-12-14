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
        "make" => Kw::Make,  // struct creation
        "self" => Kw::Self_, // method receiver
        "list" => Kw::List,  // dynamic array
        "push" => Kw::Push,
        "enum" => Kw::Enum, // enum type
        "case" => Kw::Case, // pattern match

        // Stage1 completion (I/O / errors)
        "open" => Kw::Open,
        "read" => Kw::Read,
        "save" => Kw::Write, // write is 5 letters so use save
        "shut" => Kw::Close, // close is 5 letters so use shut
        "heap" => Kw::Box,   // memory allocation
        "free" => Kw::Drop,
        "trap" => Kw::Panic, // Panic

        _ => return None,
    })
}
