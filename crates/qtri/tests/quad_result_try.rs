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

fn mk_temp_out_dir() -> PathBuf {
    let mut dir = std::env::temp_dir();
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    dir.push(format!("quadtri_test_{pid}_{nanos}"));
    std::fs::create_dir_all(&dir).expect("failed to create temp output dir");
    dir
}

#[test]
fn quad_result_try_example_runs_and_matches_output() {
    let compiler_dir = compiler_dir();
    ensure_qtrt_release_staticlib_built(&compiler_dir);
    let qtri = ensure_qtri_release_built(&compiler_dir);

    let example = repo_root()
        .join("examples")
        .join("quad")
        .join("result_try.quad");

    let out_dir = mk_temp_out_dir();
    let out_exe = out_dir.join(if cfg!(windows) {
        "quad_result_try.exe"
    } else {
        "quad_result_try"
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

    let stdout = String::from_utf8_lossy(&run.stdout).replace("\r\n", "\n");
    let lines: Vec<&str> = stdout
        .lines()
        .map(|l| l.trim())
        .filter(|l| !l.is_empty())
        .collect();

    assert_eq!(lines, vec!["5", "div by zero", "5"]);
}
