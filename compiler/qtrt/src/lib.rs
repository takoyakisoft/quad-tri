use std::ffi::CStr;
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
        let len_bytes = bytes(fp.len, elem_size);
        let cap_bytes = bytes(fp.cap, elem_size);
        Vec::from_raw_parts(fp.ptr, len_bytes, cap_bytes)
    }
}

fn store_vec(fp: &mut FatPtr, vec: Vec<u8>, elem_size: usize) {
    fp.ptr = vec.as_ptr() as *mut u8;
    fp.len = vec.len() / elem_size;
    fp.cap = vec.capacity() / elem_size;
    std::mem::forget(vec);
}

#[no_mangle]
pub extern "C" fn quad_echo_i64(x: i64) {
    println!("{x}");
}

#[no_mangle]
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

#[no_mangle]
pub extern "C" fn quad_list_new() -> FatPtr {
    FatPtr::empty()
}

#[no_mangle]
pub unsafe extern "C" fn quad_list_with_capacity(elem_size: usize, capacity: usize) -> FatPtr {
    if elem_size == 0 || capacity == 0 {
        return FatPtr::empty();
    }

    let cap_bytes = bytes(capacity, elem_size);
    let mut vec = Vec::with_capacity(cap_bytes);
    let fp = FatPtr {
        ptr: vec.as_mut_ptr(),
        len: 0,
        cap: capacity,
    };
    std::mem::forget(vec);
    fp
}

#[no_mangle]
pub unsafe extern "C" fn quad_list_push(list: *mut FatPtr, elem: *const u8, elem_size: usize) {
    if list.is_null() || elem.is_null() || elem_size == 0 {
        return;
    }

    let mut vec = reclaim_vec(*list, elem_size);
    let orig_len = vec.len();
    vec.reserve(elem_size);
    vec.set_len(orig_len + elem_size);
    copy_nonoverlapping(elem, vec.as_mut_ptr().add(orig_len), elem_size);

    store_vec(&mut *list, vec, elem_size);
    (*list).len += 1;
}

#[no_mangle]
pub unsafe extern "C" fn quad_list_drop(list: *mut FatPtr, elem_size: usize) {
    if list.is_null() || elem_size == 0 {
        return;
    }

    if (*list).cap == 0 {
        *list = FatPtr::empty();
        return;
    }

    let vec = reclaim_vec(*list, elem_size);
    drop(vec);
    *list = FatPtr::empty();
}
