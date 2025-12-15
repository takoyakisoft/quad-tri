use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::OnceLock;

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

fn ensure_qtrt_release_built(compiler_dir: &Path) {
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

fn qtri_release() -> PathBuf {
    static QTRI: OnceLock<PathBuf> = OnceLock::new();
    QTRI.get_or_init(|| {
        let dir = compiler_dir();
        ensure_qtrt_release_built(&dir);
        ensure_qtri_release_built(&dir)
    })
    .clone()
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

fn build_and_run(lang: &str, main_src: &Path, out_exe: &Path) -> Vec<String> {
    let compiler_dir = compiler_dir();
    let qtri = qtri_release();

    let build = Command::new(&qtri)
        .current_dir(&compiler_dir)
        .args([
            "build",
            "--lang",
            lang,
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

    let out_dir = out_exe.parent().unwrap();
    let run = Command::new(out_exe)
        .current_dir(out_dir)
        .output()
        .expect("failed to run built executable");

    assert!(
        run.status.success(),
        "built executable failed\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    normalize_lines(&run.stdout)
}

#[test]
fn quad_bool_keywords_work() {
    let src_dir = mk_temp_dir("qtri_bool_quad_src");
    let main_src = src_dir.join("main.quad");

    std::fs::write(
        &main_src,
        "func main() -> int:\n    cell t: bool := true\n    cell f: bool := fals\n\n    when t:\n        println(\"t is true\")\n    else:\n        println(\"t is fals\")\n\n    when f:\n        println(\"f is true\")\n    else:\n        println(\"f is fals\")\n    back 0\n",
    )
    .expect("failed to write quad source");

    let out_dir = mk_temp_dir("qtri_bool_quad_out");
    let out_exe = out_dir.join(if cfg!(windows) { "bool_quad.exe" } else { "bool_quad" });

    let lines = build_and_run("quad", &main_src, &out_exe);
    assert_eq!(lines, vec!["t is true", "f is fals"]);
}

#[test]
fn tri_bool_keywords_work() {
    let src_dir = mk_temp_dir("qtri_bool_tri_src");
    let main_src = src_dir.join("main.tri");

    std::fs::write(
        &main_src,
        "def main() -> int:\n    var t: bool := tru\n    var f: bool := fal\n\n    iff t:\n        println(\"t is tru\")\n    els:\n        println(\"t is fal\")\n\n    iff f:\n        println(\"f is tru\")\n    els:\n        println(\"f is fal\")\n    ret 0\n",
    )
    .expect("failed to write tri source");

    let out_dir = mk_temp_dir("qtri_bool_tri_out");
    let out_exe = out_dir.join(if cfg!(windows) { "bool_tri.exe" } else { "bool_tri" });

    let lines = build_and_run("tri", &main_src, &out_exe);
    assert_eq!(lines, vec!["t is tru", "f is fal"]);
}
