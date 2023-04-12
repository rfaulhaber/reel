#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

use std::ffi::CString;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

macro_rules! fset {
    ($env:expr, $name:ident, $fn:expr) => {
        let intern = (*$env)
            .intern
            .expect("could not get intern from Emacs environment!");

        let fset = intern($env, CString::new("fset").unwrap().into_raw());

        let funcall = (*$env).funcall.expect("could not get funcall");

        let symbol = intern($env, CString::new(stringify!($name)).unwrap().into_raw());

        funcall($env, fset, 2, [symbol, $fn].as_mut_ptr())
    };
}

#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(ert: *mut emacs_runtime) -> libc::c_int {
    let env = (*ert)
        .get_environment
        .expect("cannot get environment from Emacs")(ert);

    let make_function = (*env).make_function.expect("cannot get make_function!");

    todo!("Finish writing this function!")
}
