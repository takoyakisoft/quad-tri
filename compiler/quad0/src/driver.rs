use std::path::{Path, PathBuf};
use std::process::Command;

#[derive(Debug)]
pub struct DriverError(pub String);
impl std::fmt::Display for DriverError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}
impl std::error::Error for DriverError {}

fn derr(msg: impl Into<String>) -> DriverError {
    DriverError(msg.into())
}

pub fn build_exe(ll_path: &Path, out_exe: &Path) -> Result<(), DriverError> {
    let tmp_dir = std::env::temp_dir().join("quad0_build");
    std::fs::create_dir_all(&tmp_dir).map_err(|e| derr(format!("tmp dir: {e}")))?;

    let obj_path = tmp_dir.join(if cfg!(windows) { "out.obj" } else { "out.o" });
    let shim_path = tmp_dir.join("shim.rs");

    // 1) llc: .ll -> .obj
    let st = Command::new("llc")
        .arg("-O0")
        .arg("-filetype=obj")
        .arg(ll_path)
        .arg("-o")
        .arg(&obj_path)
        .status()
        .map_err(|e| derr(format!("failed to run llc: {e}")))?;
    if !st.success() {
        return Err(derr("llc failed"));
    }

    // 2) shim main
    let shim = r#"
extern "C" { fn quad_main() -> i64; }
fn main() {
    let code = unsafe { quad_main() };
    std::process::exit(code as i32);
}
"#;
    std::fs::write(&shim_path, shim).map_err(|e| derr(format!("write shim: {e}")))?;

    // 3) locate quadrt staticlib
    let (lib_dir, lib_name) = find_quadrt_lib()?;

    // 4) rustc link
    let mut cmd = Command::new("rustc");
    cmd.arg(&shim_path)
        .arg("-O")
        .arg("-o")
        .arg(out_exe);

    if cfg!(windows) {
        // Windows: 既存のままでOK
        cmd.arg("-L")
            .arg(format!("native={}", lib_dir.display()))
            .arg("-l")
            .arg(format!("static={lib_name}"))
            .arg("-C")
            .arg(format!("link-arg={}", obj_path.display()));
    } else {
        // Linux: out.o の「後ろ」に libquadrt.a を置く（順序が重要）
        let lib_path = lib_dir.join(format!("lib{lib_name}.a"));

        cmd.arg("-C").arg(format!("link-arg={}", obj_path.display()))
           .arg("-C").arg(format!("link-arg={}", lib_path.display()));
    }

    let st = cmd.status().map_err(|e| derr(format!("failed to run rustc: {e}")))?;
    if !st.success() {
        return Err(derr("rustc link failed"));
    }

    Ok(())
}

fn find_quadrt_lib() -> Result<(PathBuf, String), DriverError> {
    // quad0 の manifest dir = .../compiler/quad0
    let compiler_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .ok_or_else(|| derr("bad CARGO_MANIFEST_DIR"))?
        .to_path_buf();

    let release = compiler_dir.join("target").join("release");
    let debug = compiler_dir.join("target").join("debug");

    // MSVC: quadrt.lib / Unix: libquadrt.a
    let candidates = if cfg!(windows) {
        vec![
            (release.join("quadrt.lib"), "quadrt".to_string()),
            (debug.join("quadrt.lib"), "quadrt".to_string()),
        ]
    } else {
        vec![
            (release.join("libquadrt.a"), "quadrt".to_string()),
            (debug.join("libquadrt.a"), "quadrt".to_string()),
        ]
    };

    for (p, name) in candidates {
        if p.exists() {
            return Ok((p.parent().unwrap().to_path_buf(), name));
        }
    }

    Err(derr("quadrt staticlib not found. run: cargo build -p quadrt --release"))
}
