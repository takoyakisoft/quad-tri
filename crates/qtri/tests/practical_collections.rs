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

fn ensure_qtrt_release_built(compiler_dir: &Path) {
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
fn quad_aos_array_of_structs_works() {
    let src_dir = mk_temp_dir("qtri_practical_quad_aos_src");
    let main_src = src_dir.join("main.quad");

    // Array-of-Structs (AoS): array literal of Point, index then field.
    std::fs::write(
        &main_src,
        "type Point:\n    publ x: int\n    publ y: int\n\nfunc main() -> int:\n    cell pts: [3]Point := [\n        Point(x: 1, y: 10),\n        Point(x: 2, y: 20),\n        Point(x: 3, y: 30)\n    ]\n\n    println(pts[1].x)\n    println(pts[2].y)\n    back 0\n",
    )
    .expect("failed to write quad source");

    let out_dir = mk_temp_dir("qtri_practical_quad_aos_out");
    let out_exe = out_dir.join(if cfg!(windows) { "aos.exe" } else { "aos" });

    let lines = build_and_run("quad", &main_src, &out_exe);
    assert_eq!(lines, vec!["2", "30"]);
}

#[test]
fn quad_dict_text_to_struct_via_parallel_arrays() {
    let src_dir = mk_temp_dir("qtri_practical_quad_dict_struct_src");
    let main_src = src_dir.join("main.quad");

    // Dictionary-like lookup: keys: [text], values: [Point].
    std::fs::write(
        &main_src,
        "type Point:\n    publ x: int\n    publ y: int\n\nfunc find_y(keys: [2]text, vals: [2]Point, n: int, k: text) -> int:\n    cell i: int := 0\n    loop 1==1:\n        when i == n:\n            stop\n        when keys[i] == k:\n            back vals[i].y\n        i := i + 1\n    sys_panic(\"not found\")\n    back 0\n\nfunc main() -> int:\n    cell keys: [2]text := [\n        \"foo\",\n        \"bar\"\n    ]\n    cell vals: [2]Point := [\n        Point(x: 1, y: 11),\n        Point(x: 2, y: 22)\n    ]\n\n    println(find_y(keys, vals, 2, \"bar\"))\n    back 0\n",
    )
    .expect("failed to write quad source");

    let out_dir = mk_temp_dir("qtri_practical_quad_dict_struct_out");
    let out_exe = out_dir.join(if cfg!(windows) {
        "dict_struct.exe"
    } else {
        "dict_struct"
    });

    let lines = build_and_run("quad", &main_src, &out_exe);
    assert_eq!(lines, vec!["22"]);
}

#[test]
fn quad_dict_text_to_array_via_parallel_arrays() {
    let src_dir = mk_temp_dir("qtri_practical_quad_dict_array_src");
    let main_src = src_dir.join("main.quad");

    // Dictionary-like lookup: keys: [text], values: [[int]].
    std::fs::write(
        &main_src,
        "func find_first(keys: [2]text, vals: [2][3]int, n: int, k: text) -> int:\n    cell i: int := 0\n    loop 1==1:\n        when i == n:\n            stop\n        when keys[i] == k:\n            back vals[i][0]\n        i := i + 1\n    sys_panic(\"not found\")\n    back 0\n\nfunc main() -> int:\n    cell keys: [2]text := [\n        \"a\",\n        \"b\"\n    ]\n    cell vals: [2][3]int := [\n        [1, 2, 3],\n        [10, 20, 30]\n    ]\n\n    println(find_first(keys, vals, 2, \"b\"))\n    back 0\n",
    )
    .expect("failed to write quad source");

    let out_dir = mk_temp_dir("qtri_practical_quad_dict_array_out");
    let out_exe = out_dir.join(if cfg!(windows) {
        "dict_array.exe"
    } else {
        "dict_array"
    });

    let lines = build_and_run("quad", &main_src, &out_exe);
    assert_eq!(lines, vec!["10"]);
}

#[test]
fn quad_soa_points_using_std_vec_pointers() {
    let src_dir = mk_temp_dir("qtri_practical_quad_soa_src");
    let main_src = src_dir.join("main.quad");

    // Structure-of-Arrays (SoA): store fields in two vectors, accessed via Addr receivers.
    std::fs::write(
        &main_src,
        "from \"std/vec\"\n\ntype PointsSoA:\n    publ xs: IntVec\n    publ ys: IntVec\n\nfunc make_points() -> PointsSoA:\n    back PointsSoA(xs: IntVec.make(4), ys: IntVec.make(4))\n\nfunc main() -> int:\n    cell p: PointsSoA := make_points()\n\n    p.xs.push(1)\n    p.ys.push(10)\n    p.xs.push(2)\n    p.ys.push(20)\n\n    println(p.xs.len())\n    println(p.ys.get(1))\n\n    p.xs.free()\n    p.ys.free()\n    back 0\n",
    )
    .expect("failed to write quad source");

    let out_dir = mk_temp_dir("qtri_practical_quad_soa_out");
    let out_exe = out_dir.join(if cfg!(windows) { "soa.exe" } else { "soa" });

    let lines = build_and_run("quad", &main_src, &out_exe);
    assert_eq!(lines, vec!["2", "20"]);
}

#[test]
fn tri_aos_array_of_structs_works() {
    let src_dir = mk_temp_dir("qtri_practical_tri_aos_src");
    let main_src = src_dir.join("main.tri");

    std::fs::write(
        &main_src,
        "typ Pnt:\n    pub x: int\n    pub y: int\n\ndef main() -> int:\n    var pts: [3]Pnt := [\n        Pnt(x: 1, y: 10),\n        Pnt(x: 2, y: 20),\n        Pnt(x: 3, y: 30)\n    ]\n\n    println(pts[1].x)\n    println(pts[2].y)\n    ret 0\n",
    )
    .expect("failed to write tri source");

    let out_dir = mk_temp_dir("qtri_practical_tri_aos_out");
    let out_exe = out_dir.join(if cfg!(windows) {
        "aos_tri.exe"
    } else {
        "aos_tri"
    });

    let lines = build_and_run("tri", &main_src, &out_exe);
    assert_eq!(lines, vec!["2", "30"]);
}

#[test]
fn tri_soa_points_using_std_vec_pointers() {
    let src_dir = mk_temp_dir("qtri_practical_tri_soa_src");
    let main_src = src_dir.join("main.tri");

    std::fs::write(
        &main_src,
        "use \"std/vec\"\n\ntyp Pts:\n    pub xs: IntVec\n    pub ys: IntVec\n\ndef make_pts() -> Pts:\n    ret Pts(xs: IntVec.make(4), ys: IntVec.make(4))\n\ndef main() -> int:\n    var p: Pts := make_pts()\n\n    p.xs.push(1)\n    p.ys.push(10)\n    p.xs.push(2)\n    p.ys.push(20)\n\n    println(p.xs.len())\n    println(p.ys.get(1))\n\n    p.xs.free()\n    p.ys.free()\n    ret 0\n",
    )
    .expect("failed to write tri source");

    let out_dir = mk_temp_dir("qtri_practical_tri_soa_out");
    let out_exe = out_dir.join(if cfg!(windows) {
        "soa_tri.exe"
    } else {
        "soa_tri"
    });

    let lines = build_and_run("tri", &main_src, &out_exe);
    assert_eq!(lines, vec!["2", "20"]);
}
