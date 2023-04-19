use hyper::Method;

// method headers mode cache credentials redirect referrer-policy body

type ReelResult = Result<ReelResponse, ReelError>;

/// ReelRequest is the intermediary between Emacs and Rust.
pub struct ReelRequest {
    pub method: Method,
}

impl<T> From<hyper::Request<T>> for ReelRequest {
    fn from(value: hyper::Request<T>) -> Self {
        todo!()
    }
}

pub struct ReelResponse {}

impl<T> From<hyper::Response<T>> for ReelResponse {
    fn from(value: hyper::Response<T>) -> Self {
        todo!()
    }
}

pub struct ReelError;

pub fn execute_request(req: ReelRequest) -> ReelResult {
    todo!();
}
