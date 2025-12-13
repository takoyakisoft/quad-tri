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

        // --- Stage1 補完 (データ構造・メモリ) ---
        "impl" => Kw::Impl,
        "make" => Kw::Make, // 構造体生成
        "self" => Kw::Self_, // メソッド
        "list" => Kw::List, // 動的配列
        "push" => Kw::Push,
        "size" => Kw::Size, // 配列長
        "enum" => Kw::Enum, // 列挙型
        "case" => Kw::Case, // パターンマッチ

        // --- Stage1 補完 (I/O・エラー) ---
        "open" => Kw::Open,
        "read" => Kw::Read,
        "save" => Kw::Save, // writeは5文字なのでsave
        "shut" => Kw::Shut, // closeは5文字なのでshut
        "heap" => Kw::Heap, // メモリ確保
        "free" => Kw::Free,
        "okay" => Kw::Okay, // Result::Ok
        "fail" => Kw::Fail, // Result::Err
        "trap" => Kw::Trap, // Panic

        _ => return None,
    })
}
