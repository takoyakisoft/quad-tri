use std::ffi::CStr;
use std::io::Write;
use std::os::raw::c_char;
use std::ptr::copy_nonoverlapping;
use std::ptr::null_mut;

#[repr(C)]
#[derive(Clone, Copy)]
pub struct FatPtr {
    pub ptr: *mut u8,
    pub len: usize,
    pub cap: usize,
}

impl FatPtr {
    fn empty() -> Self {
        Self {
            ptr: null_mut(),
            len: 0,
            cap: 0,
        }
    }
}

#[inline]
unsafe fn bytes(len: usize, elem_size: usize) -> usize {
    len.checked_mul(elem_size)
        .expect("len * elem_size overflow")
}

unsafe fn reclaim_vec(fp: FatPtr, elem_size: usize) -> Vec<u8> {
    if fp.cap == 0 {
        Vec::new()
    } else {
        unsafe {
            let len_bytes = bytes(fp.len, elem_size);
            let cap_bytes = bytes(fp.cap, elem_size);
            Vec::from_raw_parts(fp.ptr, len_bytes, cap_bytes)
        }
    }
}

fn store_vec(fp: &mut FatPtr, vec: Vec<u8>, elem_size: usize) {
    fp.ptr = vec.as_ptr() as *mut u8;
    fp.len = vec.len() / elem_size;
    fp.cap = vec.capacity() / elem_size;
    std::mem::forget(vec);
}

#[unsafe(no_mangle)]
pub extern "C" fn quad_echo_i64(x: i64) {
    println!("{x}");
}

#[unsafe(no_mangle)]
pub extern "C" fn quad_print_i64(x: i64) {
    print!("{x}");
    let _ = std::io::stdout().flush();
}

#[unsafe(no_mangle)]
pub extern "C" fn quad_echo_cstr(p: *const c_char) {
    if p.is_null() {
        println!("<null>");
        return;
    }
    unsafe {
        match CStr::from_ptr(p).to_str() {
            Ok(s) => println!("{s}"),
            Err(_) => println!("<invalid utf8>"),
        }
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn quad_print_cstr(p: *const c_char) {
    if p.is_null() {
        print!("<null>");
        let _ = std::io::stdout().flush();
        return;
    }
    unsafe {
        match CStr::from_ptr(p).to_str() {
            Ok(s) => {
                print!("{s}");
                let _ = std::io::stdout().flush();
            }
            Err(_) => {
                print!("<invalid utf8>");
                let _ = std::io::stdout().flush();
            }
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_alloc(size: i64) -> *mut u8 {
    if size <= 0 {
        return null_mut();
    }

    let Ok(layout) = std::alloc::Layout::from_size_align(size as usize, 16) else {
        std::process::abort();
    };
    unsafe {
        let p = std::alloc::alloc(layout);
        if p.is_null() {
            std::process::abort();
        }
        p
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_free(ptr: *mut u8, size: i64) {
    if ptr.is_null() || size <= 0 {
        return;
    }
    let Ok(layout) = std::alloc::Layout::from_size_align(size as usize, 16) else {
        std::process::abort();
    };
    unsafe {
        std::alloc::dealloc(ptr, layout);
    }
}

#[unsafe(no_mangle)]
pub extern "C" fn quad_abort() {
    std::process::abort();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_panic(ptr: *const u8, len: i64) {
    if ptr.is_null() || len <= 0 {
        eprintln!("<panic>");
        std::process::exit(1);
    }

    unsafe {
        let slice = std::slice::from_raw_parts(ptr, len as usize);
        match std::str::from_utf8(slice) {
            Ok(s) => eprintln!("{s}"),
            Err(_) => eprintln!("<invalid utf8>"),
        }
    }
    std::process::exit(1);
}

#[unsafe(no_mangle)]
pub extern "C" fn quad_list_new() -> FatPtr {
    FatPtr::empty()
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_list_with_capacity(elem_size: usize, capacity: usize) -> FatPtr {
    if elem_size == 0 || capacity == 0 {
        return FatPtr::empty();
    }

    let cap_bytes = unsafe { bytes(capacity, elem_size) };
    let mut vec = Vec::with_capacity(cap_bytes);
    let fp = FatPtr {
        ptr: vec.as_mut_ptr(),
        len: 0,
        cap: capacity,
    };
    std::mem::forget(vec);
    fp
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_list_push(list: *mut FatPtr, elem: *const u8, elem_size: usize) {
    if list.is_null() || elem.is_null() || elem_size == 0 {
        return;
    }

    unsafe {
        let mut vec = reclaim_vec(*list, elem_size);
        let orig_len = vec.len();
        vec.reserve(elem_size);
        vec.set_len(orig_len + elem_size);
        copy_nonoverlapping(elem, vec.as_mut_ptr().add(orig_len), elem_size);

        store_vec(&mut *list, vec, elem_size);
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_list_drop(list: *mut FatPtr, elem_size: usize) {
    if list.is_null() || elem_size == 0 {
        return;
    }

    unsafe {
        // Static strings/arrays (rodata) have cap=0. Do not free.
        if (*list).cap == 0 {
            *list = FatPtr::empty();
            return;
        }

        let vec = reclaim_vec(*list, elem_size);
        drop(vec);
        *list = FatPtr::empty();
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_print_str(s: *const FatPtr) {
    unsafe {
        if s.is_null() || (*s).ptr.is_null() || (*s).len == 0 {
            return;
        }
        let slice = std::slice::from_raw_parts((*s).ptr, (*s).len);
        if let Ok(str_slice) = std::str::from_utf8(slice) {
            println!("{}", str_slice);
        } else {
            println!("<invalid utf8>");
        }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_print_str_no_nl(s: *const FatPtr) {
    unsafe {
        if s.is_null() || (*s).ptr.is_null() || (*s).len == 0 {
            return;
        }
        let slice = std::slice::from_raw_parts((*s).ptr, (*s).len);
        if let Ok(str_slice) = std::str::from_utf8(slice) {
            print!("{}", str_slice);
        } else {
            print!("<invalid utf8>");
        }
    }
    let _ = std::io::stdout().flush();
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_string_len(s: *const FatPtr) -> i64 {
    if s.is_null() {
        0
    } else {
        unsafe { (*s).len as i64 }
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_string_concat(s1: *const FatPtr, s2: *const FatPtr) -> FatPtr {
    unsafe {
        // Treat null pointers as empty strings
        let l1 = if s1.is_null() || (*s1).ptr.is_null() {
            0
        } else {
            (*s1).len
        };
        let l2 = if s2.is_null() || (*s2).ptr.is_null() {
            0
        } else {
            (*s2).len
        };

        let total_len = l1 + l2;
        if total_len == 0 {
            return FatPtr::empty();
        }

        // Allocate new buffer
        let mut vec: Vec<u8> = Vec::with_capacity(total_len);

        // Copy s1
        if l1 > 0 {
            copy_nonoverlapping((*s1).ptr, vec.as_mut_ptr(), l1);
        }

        // Copy s2
        if l2 > 0 {
            copy_nonoverlapping((*s2).ptr, vec.as_mut_ptr().add(l1), l2);
        }

        vec.set_len(total_len);

        let fp = FatPtr {
            ptr: vec.as_mut_ptr(),
            len: total_len,
            cap: total_len, // New string is heap allocated
        };
        std::mem::forget(vec);
        fp
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_string_eq(s1: *const FatPtr, s2: *const FatPtr) -> bool {
    unsafe {
        let l1 = if s1.is_null() || (*s1).ptr.is_null() {
            0
        } else {
            (*s1).len
        };
        let l2 = if s2.is_null() || (*s2).ptr.is_null() {
            0
        } else {
            (*s2).len
        };

        if l1 != l2 {
            return false;
        }
        if l1 == 0 {
            return true;
        }

        let slice1 = std::slice::from_raw_parts((*s1).ptr, l1);
        let slice2 = std::slice::from_raw_parts((*s2).ptr, l2);
        slice1 == slice2
    }
}

#[unsafe(no_mangle)]
pub unsafe extern "C" fn quad_read_file(path: *const FatPtr) -> FatPtr {
    unsafe {
        if path.is_null() || (*path).ptr.is_null() {
            eprintln!("Error: File path is null");
            std::process::exit(1);
        }

        let slice = std::slice::from_raw_parts((*path).ptr, (*path).len);
        let path_str = match std::str::from_utf8(slice) {
            Ok(s) => s,
            Err(_) => {
                eprintln!("Error: File path is not valid UTF-8");
                std::process::exit(1);
            }
        };

        match std::fs::read_to_string(path_str) {
            Ok(content) => {
                let mut vec = content.into_bytes();
                let (ptr, len, cap) = (vec.as_mut_ptr(), vec.len(), vec.capacity());
                std::mem::forget(vec);
                FatPtr { ptr, len, cap }
            }
            Err(e) => {
                eprintln!("Error: Could not read file '{}': {}", path_str, e);
                std::process::exit(1);
            }
        }
    }
}
