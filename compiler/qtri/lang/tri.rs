use crate::lex::Kw;

pub fn keyword_of(s: &str) -> Option<Kw> {
    Some(match s {
        // --- 既存 ---
        "use" => Kw::From, // import 相当（内部表現は From に寄せる）
        "typ" => Kw::Type,
        "def" => Kw::Func,
        "let" => Kw::Lock,
        "var" => Kw::Vars,
        "iff" => Kw::When,
        "elf" => Kw::Elif,
        "els" => Kw::Else,
        "for" => Kw::Loop,
        "ret" => Kw::Back,
        "prn" => Kw::Print,
        "brk" => Kw::Stop,
        "cnt" => Kw::Next,
        "ovr" => Kw::Over,

        // --- Stage1 completion (data structures / memory) ---
        "imp" => Kw::Impl,
        "new" => Kw::Make,
        "slf" => Kw::Self_,
        "vec" => Kw::List,
        "psh" => Kw::Push,
        "len" => Kw::Size,
        "enm" => Kw::Enum,
        "cas" => Kw::Case,

        // --- Stage1 completion (I/O / errors) ---
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
