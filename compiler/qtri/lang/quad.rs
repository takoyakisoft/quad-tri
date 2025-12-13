use crate::lex::Kw;

pub fn keyword_of(s: &str) -> Option<Kw> {
    Some(match s {
        // --- 既存 ---
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
        "echo" => Kw::Print,
        "stop" => Kw::Stop,
        "next" => Kw::Next,
        "over" => Kw::Over,

        // --- Stage1 completion (data structures / memory) ---
        "impl" => Kw::Impl,
        "make" => Kw::Make,  // struct creation
        "self" => Kw::Self_, // method receiver
        "list" => Kw::List,  // dynamic array
        "push" => Kw::Push,
        "size" => Kw::Size, // array length
        "enum" => Kw::Enum, // enum type
        "case" => Kw::Case, // pattern match

        // --- Stage1 completion (I/O / errors) ---
        "open" => Kw::Open,
        "read" => Kw::Read,
        "save" => Kw::Save, // write is 5 letters so use save
        "shut" => Kw::Shut, // close is 5 letters so use shut
        "heap" => Kw::Heap, // memory allocation
        "free" => Kw::Free,
        "okay" => Kw::Okay, // Result::Ok
        "fail" => Kw::Fail, // Result::Err
        "trap" => Kw::Trap, // Panic

        _ => return None,
    })
}
