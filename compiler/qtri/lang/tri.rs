use crate::lex::Kw;

pub fn keyword_of(s: &str) -> Option<Kw> {
    Some(match s {
        // Core keywords
        "use" => Kw::From, // import equivalent (normalized to From internally)
        "typ" => Kw::Type,
        "def" => Kw::Func,
        "let" => Kw::Lock,
        "var" => Kw::Vars,
        "pub" => Kw::Public,
        "prv" => Kw::Private,
        "iff" => Kw::When,
        "elf" => Kw::Elif,
        "els" => Kw::Else,
        "for" => Kw::Loop,
        "ret" => Kw::Back,
        "prn" => Kw::Print,
        "brk" => Kw::Stop,
        "nxt" => Kw::Next,
        "ovr" => Kw::Over,

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
        "wrt" => Kw::Save,
        "cls" => Kw::Shut,
        "mem" => Kw::Heap,
        "del" => Kw::Free,
        "yep" => Kw::Okay,
        "nop" => Kw::Fail,
        "die" => Kw::Trap,

        _ => return None,
    })
}
