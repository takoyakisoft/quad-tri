use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::Once;

static BUILD_QTRT: Once = Once::new();

fn repo_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .and_then(Path::parent)
        .expect("should be in crates/qtri")
        .to_path_buf()
}

fn ensure_qtrt_built() {
    BUILD_QTRT.call_once(|| {
        let root = repo_root();
        let status = Command::new("cargo")
            .current_dir(&root)
            .args(&["build", "-p", "qtrt", "--release"])
            .status()
            .expect("failed to run cargo build -p qtrt --release");
        assert!(status.success(), "qtrt build failed");
    });
}

fn find_examples(dir: &Path) -> Vec<PathBuf> {
    let mut examples = Vec::new();
    if dir.is_dir() {
        for entry in fs::read_dir(dir).unwrap() {
            let entry = entry.unwrap();
            let path = entry.path();
            if path.is_dir() {
                examples.extend(find_examples(&path));
            } else {
                let name = path.file_name().unwrap().to_str().unwrap();
                if (name.ends_with(".quad") || name.ends_with(".tri")) && !should_skip(&path) {
                    examples.push(path);
                }
            }
        }
    }
    examples.sort();
    examples
}

fn should_skip(path: &Path) -> bool {
    let name = path.file_name().unwrap().to_str().unwrap();
    if name.contains("_skip.") {
        return true;
    }
    if name.to_uppercase().starts_with("README") {
        return true;
    }

    // Skip modules unless it is main
    for component in path.components() {
        if component.as_os_str() == "modules" {
            if name != "main.quad" && name != "main.tri" {
                return true;
            }
        }
    }

    // Skip panic tests unless they have a golden file (since they crash)
    if name == "panic_need.quad" || name == "panic_req.tri" {
        let stdout_path = path.with_extension("stdout");
        if !stdout_path.exists() {
            return true;
        }
    }

    false
}

#[test]
fn run_examples_golden() {
    ensure_qtrt_built();

    let root = repo_root();
    let examples_dir = root.join("examples");
    let examples = find_examples(&examples_dir);

    let qtri_bin = env!("CARGO_BIN_EXE_qtri");
    let temp_dir = tempfile::tempdir().expect("failed to create temp dir");

    let mut failures = Vec::new();

    for example in examples {
        // println!("Running example: {:?}", example);

        let ext = example.extension().unwrap().to_str().unwrap();
        let lang = if ext == "quad" { "quad" } else { "tri" };

        let file_stem = example.file_stem().unwrap().to_str().unwrap();
        // Use a unique name to avoid collisions
        use std::time::{SystemTime, UNIX_EPOCH};
        let nanos = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let out_exe_name = if cfg!(windows) {
            format!("{}_{}.exe", file_stem, nanos)
        } else {
            format!("{}_{}", file_stem, nanos)
        };
        let out_exe = temp_dir.path().join(out_exe_name);

        // Build
        let build_output = Command::new(qtri_bin)
            .args(&[
                "build",
                "--lang",
                lang,
                example.to_str().unwrap(),
                "-o",
                out_exe.to_str().unwrap(),
            ])
            .output()
            .expect("failed to execute qtri build");

        if !build_output.status.success() {
            failures.push(format!(
                "Build failed for {:?}:\nStdout: {}\nStderr: {}",
                example,
                String::from_utf8_lossy(&build_output.stdout),
                String::from_utf8_lossy(&build_output.stderr)
            ));
            continue;
        }

        // Run
        // Set current_dir to example's directory so it can find assets like test.txt
        let run_output = Command::new(&out_exe)
            .current_dir(example.parent().unwrap())
            .output()
            .expect("failed to execute generated binary");

        let stdout_path = example.with_extension("stdout");

        let mut actual_stdout = String::from_utf8_lossy(&run_output.stdout).to_string();
        // Normalize CRLF to LF
        actual_stdout = actual_stdout.replace("\r\n", "\n");

        if stdout_path.exists() {
            let expected = fs::read_to_string(&stdout_path).expect("failed to read stdout file");
            let expected = expected.replace("\r\n", "\n");

            if actual_stdout.trim_end() != expected.trim_end() {
                if env::var("UPDATE_GOLDEN").is_ok() {
                    fs::write(&stdout_path, actual_stdout).expect("failed to write stdout file");
                } else {
                    failures.push(format!(
                        "Stdout mismatch for {:?}:\nExpected:\n---\n{}\n---\nActual:\n---\n{}\n---",
                        example,
                        expected.trim_end(),
                        actual_stdout.trim_end()
                    ));
                }
            }
        } else {
            // Default check: exit code 0
            // If UPDATE_GOLDEN is set, we generate the file regardless of exit code (e.g. for structs.quad which returns 22)
            if env::var("UPDATE_GOLDEN").is_ok() {
                fs::write(&stdout_path, actual_stdout).expect("failed to create stdout file");
            } else if !run_output.status.success() {
                failures.push(format!(
                    "Execution failed (exit code {:?}) for {:?}:\nStdout: {}\nStderr: {}",
                    run_output.status.code(),
                    example,
                    actual_stdout,
                    String::from_utf8_lossy(&run_output.stderr)
                ));
            }
        }
    }

    if !failures.is_empty() {
        panic!("Some examples failed:\n\n{}", failures.join("\n\n"));
    }
}
