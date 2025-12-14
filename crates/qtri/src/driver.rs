use crate::ast::LinkedProgram;
use crate::emit_cranelift;
use crate::sem::SemInfo;
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

pub fn build_exe(prog: &LinkedProgram, sem: &SemInfo, out_exe: &Path) -> Result<(), DriverError> {
    let pid = std::process::id();
    let nanos = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map_err(|e| derr(format!("tmp dir timestamp: {e}")))?
        .as_nanos();
    let tmp_dir = std::env::temp_dir().join(format!("quad0_build_{pid}_{nanos}"));
    std::fs::create_dir_all(&tmp_dir).map_err(|e| derr(format!("tmp dir: {e}")))?;

    let obj_path = tmp_dir.join(if cfg!(windows) { "out.obj" } else { "out.o" });
    let shim_path = tmp_dir.join("shim.rs");

    // 1) Cranelift: generate object file
    let obj_bytes = emit_cranelift::emit_module(prog, sem)
        .map_err(|e| derr(format!("cranelift emit error: {e}")))?;

    std::fs::write(&obj_path, obj_bytes).map_err(|e| derr(format!("write obj: {e}")))?;

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
    cmd.arg(&shim_path).arg("-O").arg("-o").arg(out_exe);

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

        cmd.arg("-C")
            .arg(format!("link-arg={}", obj_path.display()))
            .arg("-C")
            .arg(format!("link-arg={}", lib_path.display()))
            .arg("-C")
            .arg("link-arg=-no-pie");
    }

    let st = cmd
        .status()
        .map_err(|e| derr(format!("failed to run rustc: {e}")))?;
    if !st.success() {
        return Err(derr("rustc link failed"));
    }

    Ok(())
}

fn find_quadrt_lib() -> Result<(PathBuf, String), DriverError> {
    // CARGO_MANIFEST_DIR points at the qtri crate directory.
    // We want the workspace root (repo root), so go up: crates/qtri -> crates -> <repo>.
    let qtri_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let crates_dir = qtri_dir
        .parent()
        .ok_or_else(|| derr("bad CARGO_MANIFEST_DIR"))?
        .to_path_buf();
    let workspace_root = crates_dir
        .parent()
        .ok_or_else(|| derr("bad CARGO_MANIFEST_DIR"))?
        .to_path_buf();

    // Deterministic resolution:
    // 1) Optional explicit override via QTRT_LIB_DIR
    // 2) Otherwise, workspace_root/target/release
    let lib_name = "qtrt".to_string();

    if let Ok(dir) = std::env::var("QTRT_LIB_DIR") {
        let dir = PathBuf::from(dir);
        let lib_path = if cfg!(windows) {
            dir.join("qtrt.lib")
        } else {
            dir.join("libqtrt.a")
        };
        if lib_path.exists() {
            return Ok((dir, lib_name));
        }
    }

    let dir = workspace_root.join("target").join("release");
    let lib_path = if cfg!(windows) {
        dir.join("qtrt.lib")
    } else {
        dir.join("libqtrt.a")
    };
    if lib_path.exists() {
        return Ok((dir, lib_name));
    }

    Err(derr(
        "qtrt staticlib not found. run: cargo build -p qtrt --release",
    ))
}
