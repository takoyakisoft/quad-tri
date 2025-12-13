mod lex;
mod ast;
mod parse;
mod emit_cranelift;
mod driver;
mod sem;

#[path = "../lang/quad.rs"]
mod quad;
#[path = "../lang/tri.rs"]
mod tri;

use std::path::PathBuf;
use lex::Language;

fn print_usage() {
    eprintln!("usage:");
    eprintln!("  quad0 lex   --lang <quad|tri> <file>");
    eprintln!("  quad0 build --lang <quad|tri> <file> -o <out.exe> [--emit-llvm <out.ll>]");
}

fn parse_lang(s: &str) -> Option<Language> {
    match s {
        "quad" => Some(Language::Quad),
        "tri" => Some(Language::Tri),
        _ => None,
    }
}

fn main() {
    let mut args = std::env::args().skip(1).collect::<Vec<_>>();
    if args.is_empty() {
        print_usage();
        std::process::exit(2);
    }

    let cmd = args.remove(0);
    match cmd.as_str() {
        "lex" => cmd_lex(args),
        "build" => cmd_build(args),
        _ => {
            print_usage();
            std::process::exit(2);
        }
    }
}

fn cmd_lex(args: Vec<String>) {
    let (lang, file) = parse_common_lang_file(args).unwrap_or_else(|m| {
        eprintln!("{m}");
        print_usage();
        std::process::exit(2);
    });

    match lex::lex_file(lang, &file) {
        Ok(tokens) => {
            for t in tokens {
                println!("{:>4}:{:<3}  {:?}", t.span.line, t.span.col, t.kind);
            }
        }
        Err(e) => {
            eprintln!("lex error: {e}");
            std::process::exit(1);
        }
    }
}

fn cmd_build(args: Vec<String>) {
    let mut lang = Language::Quad;
    let mut file: Option<PathBuf> = None;
    let mut out_exe: Option<PathBuf> = None;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--lang" => {
                i += 1;
                if i >= args.len() { die("--lang needs value"); }
                lang = parse_lang(&args[i]).unwrap_or_else(|| die("unknown lang"));
            }
            "-o" => {
                i += 1;
                if i >= args.len() { die("-o needs value"); }
                out_exe = Some(PathBuf::from(&args[i]));
            }
            x if x.starts_with('-') => die(&format!("unknown option: {x}")),
            x => file = Some(PathBuf::from(x)),
        }
        i += 1;
    }

    let file = file.unwrap_or_else(|| die("missing <file>"));
    let out_exe = out_exe.unwrap_or_else(|| die("missing -o <out.exe>"));

    let tokens = lex::lex_file(lang, &file).unwrap_or_else(|e| die(&format!("lex error: {e}")));
    let prog = parse::parse_program(&tokens).unwrap_or_else(|e| die(&format!("parse error: {e}")));
    let sem_info = sem::check(&prog).unwrap_or_else(|e| die(&format!("sem error: {e}")));

    driver::build_exe(&prog, &sem_info, &out_exe).unwrap_or_else(|e| die(&format!("link error: {e}")));
    println!("built: {}", out_exe.display());
}

fn parse_common_lang_file(args: Vec<String>) -> Result<(Language, PathBuf), String> {
    let mut lang = Language::Quad;
    let mut file: Option<PathBuf> = None;

    let mut i = 0;
    while i < args.len() {
        match args[i].as_str() {
            "--lang" => {
                i += 1;
                if i >= args.len() {
                    return Err("missing value for --lang".into());
                }
                lang = parse_lang(&args[i]).ok_or_else(|| format!("unknown lang: {}", args[i]))?;
            }
            x if x.starts_with('-') => return Err(format!("unknown option: {x}")),
            x => file = Some(PathBuf::from(x)),
        }
        i += 1;
    }

    let file = file.ok_or_else(|| "missing <file>".to_string())?;
    Ok((lang, file))
}

fn die(msg: &str) -> ! {
    eprintln!("{msg}");
    std::process::exit(1);
}
