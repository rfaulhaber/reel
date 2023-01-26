#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

#[no_mangle]
#[allow(non_upper_case_globals)]
pub static plugin_is_GPL_compatible: libc::c_int = 0;

#[no_mangle]
pub unsafe extern "C" fn emacs_module_init(ert: *mut emacs_runtime) -> libc::c_int {
    let env = (*ert)
        .get_environment
        .expect("cannot get environment from Emacs")(ert);

    // to be used later
    let make_function = (*env).make_function.expect("cannot get make_function!");

    todo!("Finish writing this function!")
}
