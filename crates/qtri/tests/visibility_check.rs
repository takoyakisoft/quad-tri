use std::path::{Path, PathBuf};
use std::process::Command;
use std::fs;

fn compiler_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("qtri should live under <repo>/crates/qtri")
        .to_path_buf()
}

fn ensure_qtrt_release_staticlib_built(compiler_dir: &Path) {
    // Force rebuild with PIC
    let _ = Command::new("cargo")
        .current_dir(compiler_dir)
        .args(["clean", "-p", "qtrt"])
        .status();

    let st = Command::new("cargo")
        .current_dir(compiler_dir)
        .env("RUSTFLAGS", "-C relocation-model=pic")
        .args(["build", "-p", "qtrt", "--release"])
        .status()
        .expect("failed to spawn cargo build -p qtrt --release");
    assert!(st.success(), "cargo build -p qtrt --release failed");
}

fn ensure_qtri_release_built(compiler_dir: &Path) -> PathBuf {
    let qtri_release = compiler_dir
        .join("target")
        .join("release")
        .join(if cfg!(windows) { "qtri.exe" } else { "qtri" });
    let st = Command::new("cargo")
        .current_dir(compiler_dir)
        .args(["build", "-p", "qtri", "--release"])
        .status()
        .expect("failed to spawn cargo build -p qtri --release");
    assert!(st.success(), "cargo build -p qtri --release failed");

    assert!(qtri_release.exists(), "qtri release binary not found");
    qtri_release
}

fn mk_temp_dir() -> PathBuf {
    let mut dir = std::env::temp_dir();
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    dir.push(format!("qtri_test_{}_{}", pid, nanos));
    fs::create_dir_all(&dir).expect("failed to create temp dir");
    dir
}

fn write_file(dir: &Path, name: &str, content: &str) -> PathBuf {
    let path = dir.join(name);
    fs::write(&path, content).expect("failed to write file");
    path
}

#[test]
fn quad_visibility_check() {
    let compiler_dir = compiler_dir();
    ensure_qtrt_release_staticlib_built(&compiler_dir);
    let qtri = ensure_qtri_release_built(&compiler_dir);

    let temp_dir = mk_temp_dir();

    // Define a module with public and private fields
    write_file(&temp_dir, "lib.quad", r#"
type Point:
    publ x: int
    priv y: int

func make_point(x: int, y: int) -> Point:
    back Point {
        x: x,
        y: y
    }
"#);

    // Case 1: Accessing public field should succeed
    write_file(&temp_dir, "main_pub.quad", r#"
from "lib"

func main() -> int:
    cell p: Point := make_point(10, 20)
    println(p.x)
    back 0
"#);

    let out_pub = temp_dir.join("main_pub");
    let build_pub = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "quad",
            temp_dir.join("main_pub.quad").to_str().unwrap(),
            "-o",
            out_pub.to_str().unwrap(),
        ])
        .output()
        .expect("failed to run qtri build");

    assert!(
        build_pub.status.success(),
        "Public access failed: {}",
        String::from_utf8_lossy(&build_pub.stderr)
    );

    // Case 2: Accessing private field SHOULD fail, but currently succeeds (bug)
    write_file(&temp_dir, "main_priv.quad", r#"
from "lib"

func main() -> int:
    cell p: Point := make_point(10, 20)
    # This should be a compile error because y is private
    println(p.y)
    back 0
"#);

    let out_priv = temp_dir.join("main_priv");
    let build_priv = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "quad",
            temp_dir.join("main_priv.quad").to_str().unwrap(),
            "-o",
            out_priv.to_str().unwrap(),
        ])
        .output()
        .expect("failed to run qtri build");

    if build_priv.status.success() {
        panic!(
            "Private field access succeeded unexpectedly:\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build_priv.stdout),
            String::from_utf8_lossy(&build_priv.stderr)
        );
    } else {
        let stderr = String::from_utf8_lossy(&build_priv.stderr);
        assert!(
            stderr.contains("field y is private"),
            "Expected 'field y is private' error, got:\n{}",
            stderr
        );
    }

    // Case 3: Accessing private field within the same module should succeed
    write_file(&temp_dir, "same_module.quad", r#"
type Point:
    publ x: int
    priv y: int

func main() -> int:
    cell p: Point := Point {
        x: 10,
        y: 20
    }
    # Private field access within the same module should work
    println(p.y)
    back 0
"#);

    let out_same_module = temp_dir.join("same_module");
    let build_same_module = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "quad",
            temp_dir.join("same_module.quad").to_str().unwrap(),
            "-o",
            out_same_module.to_str().unwrap(),
        ])
        .output()
        .expect("failed to run qtri build");

    assert!(
        build_same_module.status.success(),
        "Private access within same module should succeed: {}",
        String::from_utf8_lossy(&build_same_module.stderr)
    );

    // Case 4: Struct literal initialization with private fields from another module should fail
    write_file(&temp_dir, "main_literal.quad", r#"
from "lib"

func main() -> int:
    # This should be a compile error because y is private
    cell p: Point := Point {
        x: 10,
        y: 20
    }
    back 0
"#);

    let out_literal = temp_dir.join("main_literal");
    let build_literal = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "quad",
            temp_dir.join("main_literal.quad").to_str().unwrap(),
            "-o",
            out_literal.to_str().unwrap(),
        ])
        .output()
        .expect("failed to run qtri build");

    if build_literal.status.success() {
        panic!(
            "Struct literal with private field succeeded unexpectedly:\nstdout:\n{}\nstderr:\n{}",
            String::from_utf8_lossy(&build_literal.stdout),
            String::from_utf8_lossy(&build_literal.stderr)
        );
    } else {
        let stderr = String::from_utf8_lossy(&build_literal.stderr);
        assert!(
            stderr.contains("field y is private"),
            "Expected 'field y is private' error for struct literal, got:\n{}",
            stderr
        );
    }
}
