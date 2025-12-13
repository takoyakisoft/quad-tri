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
    let mut structs = Vec::new();
    let mut funcs = Vec::new();
    let mut visited = HashSet::<PathBuf>::new();
    load_one(
        lang,
        entry,
        None,
        &mut visited,
        &mut structs,
        &mut funcs,
    )?;
    Ok(LinkedProgram { structs, funcs })
}

fn append_default_extension(lang: Language, path: &Path) -> PathBuf {
    if path.extension().is_some() {
        return path.to_path_buf();
    }

    let mut with_ext = path.to_path_buf();
    let ext = match lang {
        Language::Quad => "quad",
        Language::Tri => "tri",
    };

    with_ext.set_extension(ext);
    with_ext
}

fn load_one(
    lang: Language,
    path: &Path,
    import_span: Option<Span>,
    visited: &mut HashSet<PathBuf>,
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

    let tokens = lex::lex_file(lang, &canonical).map_err(|err| LoadError::Lex {
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
        let target = base_dir.join(&import.path);
        let target = append_default_extension(lang, &target);
        load_one(lang, &target, Some(import.span), visited, structs, funcs)?;
    }

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

        let lib = write_file(root, "lib.quad", "func helper() -> intg:\n    back 1\n");
        let entry = write_file(
            root,
            "main.quad",
            "from \"lib\"\n\nfunc main() -> intg:\n    back helper()\n",
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

        write_file(root, "pkg/util.quad", "func util() -> intg:\n    back 2\n");
        write_file(
            root,
            "pkg/mod.quad",
            "from \"util\"\n\nfunc mid() -> intg:\n    back util()\n",
        );
        let entry = write_file(
            root,
            "main.quad",
            "from \"pkg/mod\"\n\nfunc main() -> intg:\n    back mid()\n",
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
}
