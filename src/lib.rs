#![allow(non_upper_case_globals)]
#![allow(non_camel_case_types)]
#![allow(non_snake_case)]

mod request;

use std::ffi::CString;

include!(concat!(env!("OUT_DIR"), "/bindings.rs"));

macro_rules! fset {
    ($env:expr, $name:expr, $fn:expr) => {
        let intern = (*$env)
            .intern
            .expect("could not get intern from Emacs environment!");

        let fset = intern($env, CString::new("fset").unwrap().into_raw());

        let funcall = (*$env).funcall.expect("could not get funcall");

        let symbol = intern(
            $env,
            CString::new($name)
                .expect("could not invoke CString::new")
                .into_raw(),
        );

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

    let reel_execute_request_fn = make_function(
        env,
        9,
        9,
        Some(reel_execute_request),
        CString::new("Executes an HTTP request.")
            .expect("could not invoke CString::new")
            .into_raw(),
        std::ptr::null_mut(),
    );

    fset!(env, "reel--execute-request", reel_execute_request_fn);

    todo!("Finish writing this function!")
}

pub unsafe extern "C" fn reel_execute_request(
    env: *mut emacs_env,
    nargs: isize,
    args: *mut emacs_value,
    data: *mut std::os::raw::c_void,
) -> emacs_value {
    let req_builder = hyper::Request::builder();

    todo!("finish!");
}

unsafe fn get_req_from_emacs_value(args: *mut emacs_value) -> Result<(), ()> {
    let url = *args.offset(0);
    let method = *args.offset(1);
    let headers = *args.offset(1);

    todo!();
}
