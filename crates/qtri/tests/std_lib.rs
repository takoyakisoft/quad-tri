use std::path::{Path, PathBuf};
use std::process::Command;

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("qtri should live under <repo>/crates/qtri")
        .to_path_buf()
}

fn compiler_dir() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("qtri should live under <repo>/crates/qtri")
        .to_path_buf()
}

fn cargo_target_dir(compiler_dir: &Path) -> PathBuf {
    std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .unwrap_or_else(|| compiler_dir.join("target"))
}

fn ci_prebuilt() -> bool {
    std::env::var_os("QTRI_CI_PREBUILT").is_some()
}

fn ensure_qtrt_release_staticlib_built(compiler_dir: &Path) {
    let release_dir = cargo_target_dir(compiler_dir).join("release");
    let lib_path = if cfg!(windows) {
        release_dir.join("qtrt.lib")
    } else {
        release_dir.join("libqtrt.a")
    };
    if ci_prebuilt() && lib_path.exists() {
        return;
    }

    let st = Command::new("cargo")
        .current_dir(compiler_dir)
        .args(["build", "-p", "qtrt", "--release"])
        .status()
        .expect("failed to spawn cargo build -p qtrt --release");
    assert!(st.success(), "cargo build -p qtrt --release failed");
}

fn ensure_qtri_release_built(compiler_dir: &Path) -> PathBuf {
    let qtri_release = cargo_target_dir(compiler_dir)
        .join("release")
        .join(if cfg!(windows) { "qtri.exe" } else { "qtri" });

    if ci_prebuilt() && qtri_release.exists() {
        return qtri_release;
    }

    let st = Command::new("cargo")
        .current_dir(compiler_dir)
        .args(["build", "-p", "qtri", "--release"])
        .status()
        .expect("failed to spawn cargo build -p qtri --release");
    assert!(st.success(), "cargo build -p qtri --release failed");

    assert!(qtri_release.exists(), "qtri release binary not found");
    qtri_release
}

fn mk_temp_out_dir(prefix: &str) -> PathBuf {
    let mut dir = std::env::temp_dir();
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    dir.push(format!("{prefix}_{pid}_{nanos}"));
    std::fs::create_dir_all(&dir).expect("failed to create temp output dir");
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
fn quad_std_example_runs_and_matches_output() {
    let compiler_dir = compiler_dir();
    ensure_qtrt_release_staticlib_built(&compiler_dir);
    let qtri = ensure_qtri_release_built(&compiler_dir);

    let example = repo_root().join("examples").join("test_std.quad");

    let out_dir = mk_temp_out_dir("quadtri_std_quad");
    let out_exe = out_dir.join(if cfg!(windows) {
        "std_quad.exe"
    } else {
        "std_quad"
    });

    let build = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "quad",
            example.to_str().unwrap(),
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
    assert_eq!(
        lines,
        vec![
            "Testing Std Lib",
            "Vec len:",
            "3",
            "Vec[1]:",
            "20",
            "Map foo:",
            "42",
            "baz not found (ok)",
            "Parsed:",
            "12345",
        ]
    );
}

#[test]
fn tri_can_use_quad_std_vec_via_use() {
    let compiler_dir = compiler_dir();
    ensure_qtrt_release_staticlib_built(&compiler_dir);
    let qtri = ensure_qtri_release_built(&compiler_dir);

    let src_dir = mk_temp_out_dir("quadtri_std_tri_src");
    let src = src_dir.join("main.tri");
    std::fs::write(
        &src,
        "use \"std/vec\"\n\n\ndef main() -> int:\n    var v: IntVec := IntVec.make(4)\n    v.push(10)\n    v.push(20)\n    println(v.len())\n    println(v.get(1))\n    ret 0\n",
    )
    .expect("failed to write tri source");

    let out_dir = mk_temp_out_dir("quadtri_std_tri_out");
    let out_exe = out_dir.join(if cfg!(windows) {
        "std_tri.exe"
    } else {
        "std_tri"
    });

    let build = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            "tri",
            src.to_str().unwrap(),
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
    assert_eq!(lines, vec!["2", "20"]);
}
