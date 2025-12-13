use crate::lex::Kw;

pub fn keyword_of(s: &str) -> Option<Kw> {
    Some(match s {
        "from" => Kw::From,
        "type" => Kw::Type,
        "func" => Kw::Func,
        "lock" => Kw::Lock,
        "vars" => Kw::Vars,
        "when" => Kw::When,
        "elif" => Kw::Elif,
        "else" => Kw::Else,
        "loop" => Kw::Loop,
        "back" => Kw::Back,
        "echo" => Kw::Echo,
        "stop" => Kw::Stop,
        "next" => Kw::Next,
        "over" => Kw::Over,
        _ => return None,
    })
}
