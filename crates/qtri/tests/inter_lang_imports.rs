use std::path::{Path, PathBuf};
use std::process::Command;

fn compiler_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("qtri should live under <repo>/crates/qtri")
        .to_path_buf()
}

fn ensure_qtrt_release_staticlib_built(compiler_dir: &Path) {
    let st = Command::new("cargo")
        .current_dir(compiler_dir)
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

fn mk_temp_dir(prefix: &str) -> PathBuf {
    let mut dir = std::env::temp_dir();
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    dir.push(format!("{prefix}_{pid}_{nanos}"));
    std::fs::create_dir_all(&dir).expect("failed to create temp dir");
    dir
}

fn normalize_lines(stdout: &[u8]) -> Vec<String> {
    let s = String::from_utf8_lossy(stdout).replace("\r\n", "\n");
    s.lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .map(|l| l.to_string())
        .collect()
}

#[test]
fn quad_can_from_import_tri_module() {
    let compiler_dir = compiler_dir();
    ensure_qtrt_release_staticlib_built(&compiler_dir);
    let qtri = ensure_qtri_release_built(&compiler_dir);

    let src_dir = mk_temp_dir("quadtri_interop_quad_src");

    std::fs::write(
        src_dir.join("lib.tri"),
        "def add1(x: int) -> int:\n    ret x + 1\n",
    )
    .expect("failed to write lib.tri");

    let main_src = src_dir.join("main.quad");
    std::fs::write(
        &main_src,
        "from \"lib\"\n\nfunc main() -> int:\n    println(add1(41))\n    back 0\n",
    )
    .expect("failed to write main.quad");

    let out_dir = mk_temp_dir("quadtri_interop_quad_out");
    let out_exe = out_dir.join(if cfg!(windows) {
        "interop_quad.exe"
    } else {
        "interop_quad"
    });

    let build = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "quad",
            main_src.to_str().unwrap(),
            "-o",
            out_exe.to_str().unwrap(),
        ])
        .output()
        .expect("failed to run qtri build");

    assert!(
        build.status.success(),
        "qtri build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&out_exe)
        .current_dir(&out_dir)
        .output()
        .expect("failed to run built executable");
    assert!(
        run.status.success(),
        "built executable failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    let lines = normalize_lines(&run.stdout);
    assert_eq!(lines, vec!["42"]);
}

#[test]
fn tri_can_use_import_quad_module() {
    let compiler_dir = compiler_dir();
    ensure_qtrt_release_staticlib_built(&compiler_dir);
    let qtri = ensure_qtri_release_built(&compiler_dir);

    let src_dir = mk_temp_dir("quadtri_interop_tri_src");

    std::fs::write(
        src_dir.join("lib.quad"),
        "func add2(x: int) -> int:\n    back x + 2\n",
    )
    .expect("failed to write lib.quad");

    let main_src = src_dir.join("main.tri");
    std::fs::write(
        &main_src,
        "use \"lib\"\n\n\ndef main() -> int:\n    println(add2(40))\n    ret 0\n",
    )
    .expect("failed to write main.tri");

    let out_dir = mk_temp_dir("quadtri_interop_tri_out");
    let out_exe = out_dir.join(if cfg!(windows) {
        "interop_tri.exe"
    } else {
        "interop_tri"
    });

    let build = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "tri",
            main_src.to_str().unwrap(),
            "-o",
            out_exe.to_str().unwrap(),
        ])
        .output()
        .expect("failed to run qtri build");

    assert!(
        build.status.success(),
        "qtri build failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = Command::new(&out_exe)
        .current_dir(&out_dir)
        .output()
        .expect("failed to run built executable");
    assert!(
        run.status.success(),
        "built executable failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    let lines = normalize_lines(&run.stdout);
    assert_eq!(lines, vec!["42"]);
}
