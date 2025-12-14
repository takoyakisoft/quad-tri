use std::collections::HashSet;
use std::path::{Path, PathBuf};

use crate::ast::LinkedProgram;
use crate::lex::{self, Language, Span};
use crate::parse;

#[derive(Debug)]
pub enum LoadError {
    Lex {
        path: PathBuf,
        err: lex::LexError,
    },
    Parse {
        path: PathBuf,
        err: parse::ParseError,
    },
    Io {
        path: PathBuf,
        span: Option<Span>,
        err: std::io::Error,
    },
}

impl std::fmt::Display for LoadError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoadError::Lex { path, err } => write!(f, "{}: lex error: {}", path.display(), err),
            LoadError::Parse { path, err } => write!(f, "{}: parse error: {}", path.display(), err),
            LoadError::Io { path, span, err } => match span {
                Some(sp) => write!(
                    f,
                    "{}:{}:{}: io error: {}",
                    path.display(),
                    sp.line,
                    sp.col,
                    err
                ),
                None => write!(f, "{}: io error: {}", path.display(), err),
            },
        }
    }
}

impl std::error::Error for LoadError {}

pub fn load_program(lang: Language, entry: &Path) -> Result<LinkedProgram, LoadError> {
    let mut enums = Vec::new();
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    let mut visited = HashSet::<PathBuf>::new();
    let mut source_map = Vec::new();

    // Resolve entry point extension if needed
    let entry_resolved = resolve_extension_check_exists(lang, entry);

    load_one(
        lang,
        &entry_resolved,
        None,
        &mut visited,
        &mut source_map,
        &mut enums,
        &mut structs,
        &mut funcs,
    )?;
    Ok(LinkedProgram {
        enums,
        structs,
        funcs,
        source_map,
    })
}

fn resolve_extension_check_exists(lang: Language, path: &Path) -> PathBuf {
    if path.extension().is_some() {
        return path.to_path_buf();
    }

    let quad = path.with_extension("quad");
    let tri = path.with_extension("tri");

    if quad.exists() {
        return quad;
    }
    if tri.exists() {
        return tri;
    }

    match lang {
        Language::Quad => quad,
        Language::Tri => tri,
    }
}

fn load_one(
    lang: Language,
    path: &Path,
    import_span: Option<Span>,
    visited: &mut HashSet<PathBuf>,
    source_map: &mut Vec<PathBuf>,
    enums: &mut Vec<crate::ast::EnumDef>,
    structs: &mut Vec<crate::ast::StructDef>,
    funcs: &mut Vec<crate::ast::Func>,
) -> Result<(), LoadError> {
    let canonical = std::fs::canonicalize(path).map_err(|e| LoadError::Io {
        path: path.to_path_buf(),
        span: import_span,
        err: e,
    })?;

    if !visited.insert(canonical.clone()) {
        return Ok(());
    }

    // Determine language from file extension
    let file_lang = if let Some(ext) = canonical.extension().and_then(|s| s.to_str()) {
        match ext {
            "quad" => Language::Quad,
            "tri" => Language::Tri,
            _ => lang,
        }
    } else {
        lang
    };

    let file_id = source_map.len();
    source_map.push(canonical.clone());

    let src = std::fs::read_to_string(&canonical).map_err(|e| LoadError::Io {
        path: canonical.clone(),
        span: import_span,
        err: e,
    })?;

    let tokens = lex::lex_str(file_lang, &src, file_id).map_err(|err| LoadError::Lex {
        path: canonical.clone(),
        err,
    })?;
    let parsed = parse::parse_program(&tokens).map_err(|err| LoadError::Parse {
        path: canonical.clone(),
        err,
    })?;

    let base_dir = canonical
        .parent()
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("."));

    for import in parsed.imports {
        let relative_target = base_dir.join(&import.path);
        let mut target = resolve_extension_check_exists(file_lang, &relative_target);

        // If not found, try std (if import path looks like std/...)
        if !target.exists() {
            if import.path.starts_with("std") {
                let std_target_base = PathBuf::from(&import.path);
                let std_target = resolve_extension_check_exists(file_lang, &std_target_base);
                if std_target.exists() {
                    target = std_target;
                }
            }
        }

        load_one(
            file_lang,
            &target,
            Some(import.span),
            visited,
            source_map,
            enums,
            structs,
            funcs,
        )?;
    }

    enums.extend(parsed.enums);
    structs.extend(parsed.structs);
    funcs.extend(parsed.funcs);
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    use tempfile::tempdir;

    fn write_file(base: &Path, rel: &str, contents: &str) -> PathBuf {
        let full = base.join(rel);
        if let Some(parent) = full.parent() {
            fs::create_dir_all(parent).unwrap();
        }
        fs::write(&full, contents).unwrap();
        full
    }

    #[test]
    fn loads_imported_modules_before_entry_functions() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        let lib = write_file(root, "lib.quad", "func helper() -> int:\n    back 1\n");
        let entry = write_file(
            root,
            "main.quad",
            "from \"lib\"\n\nfunc main() -> int:\n    back helper()\n",
        );

        let program = load_program(Language::Quad, &entry).expect("program loads");
        let names: Vec<_> = program.funcs.iter().map(|f| f.name.as_str()).collect();

        assert!(lib.exists());
        assert_eq!(names, ["helper", "main"]);
    }

    #[test]
    fn resolves_nested_imports_relative_to_each_module() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        write_file(root, "pkg/util.quad", "func util() -> int:\n    back 2\n");
        write_file(
            root,
            "pkg/mod.quad",
            "from \"util\"\n\nfunc mid() -> int:\n    back util()\n",
        );
        let entry = write_file(
            root,
            "main.quad",
            "from \"pkg/mod\"\n\nfunc main() -> int:\n    back mid()\n",
        );

        let program = load_program(Language::Quad, &entry).expect("program loads");
        let names: Vec<_> = program.funcs.iter().map(|f| f.name.as_str()).collect();

        assert_eq!(names, ["util", "mid", "main"]);
    }

    #[test]
    fn handles_cycles_without_duplicate_loading() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        let a = write_file(root, "a.tri", "use \"b\"\n\ndef a() -> int:\n    ret 1\n");
        write_file(root, "b.tri", "use \"a\"\n\ndef b() -> int:\n    ret 2\n");

        let program = load_program(Language::Tri, &a).expect("program loads");
        let names: Vec<_> = program.funcs.iter().map(|f| f.name.as_str()).collect();

        assert_eq!(names, ["b", "a"]);
    }

    #[test]
    fn loads_mixed_languages() {
        let dir = tempdir().unwrap();
        let root = dir.path();

        // Quad imports Tri
        write_file(root, "helper.tri", "def help() -> int:\n    ret 42\n");
        let entry = write_file(
            root,
            "main.quad",
            "from \"helper\"\n\nfunc main() -> int:\n    back help()\n",
        );

        let program = load_program(Language::Quad, &entry).expect("program loads");
        let names: Vec<_> = program.funcs.iter().map(|f| f.name.as_str()).collect();

        assert_eq!(names, ["help", "main"]);
    }
}
