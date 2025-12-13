use crate::lex::Kw;

pub fn keyword_of(s: &str) -> Option<Kw> {
    Some(match s {
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
        "prn" => Kw::Echo,
        "brk" => Kw::Stop,
        "cnt" => Kw::Next,
        "ovr" => Kw::Over,
        _ => return None,
    })
}
