use std::ffi::CStr;
use std::os::raw::c_char;

#[no_mangle]
pub extern "C" fn quad_echo_i64(x: i64) {
    println!("{x}");
}

#[no_mangle]
pub extern "C" fn quad_echo_cstr(p: *const c_char) {
    if p.is_null() { println!("<null>"); return; }
    unsafe {
        match CStr::from_ptr(p).to_str() {
            Ok(s) => println!("{s}"),
            Err(_) => println!("<invalid utf8>"),
        }
    }
}
