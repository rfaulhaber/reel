use hyper::Method;

// method headers mode cache credentials redirect referrer-policy body

type ReelResult = Result<ReelResponse, ReelError>;

pub struct ReelRequest {
    pub method: Method,
}

pub struct ReelResponse {}

pub struct ReelError;

pub fn execute_request() -> ReelResult {
    todo!();
}
