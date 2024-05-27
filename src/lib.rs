use emacs::{Env, FromLisp, IntoLisp, Result as EmacsResult, Value};
use reqwest::header::{HeaderMap, HeaderName};

use std::cell::OnceCell;

use tokio::runtime::Runtime;

const TOKIO_RUNTIME: OnceCell<Runtime> = OnceCell::new();

emacs::plugin_is_GPL_compatible!();

/// Intermediate value, converted into a `reqwest::Request`.
#[derive(Debug)]
pub struct ReelRequest {
    pub url: reqwest::Url,
    pub method: reqwest::Method,
    pub headers: reqwest::header::HeaderMap,
    pub body: reqwest::Body,
}

impl<'e> FromLisp<'e> for ReelRequest {
    fn from_lisp(value: emacs::Value<'e>) -> EmacsResult<Self> {
        println!("converting value");
        let env = value.env;
        let url = env.call("nth", (0, value))?.into_rust::<String>()?;
        let method = env.call("nth", (1, value))?.into_rust::<String>()?;
        let headers_value: emacs::Value = env.call("nth", (2, value))?;
        let headers = get_headers_from_alist(headers_value)?;
        let body = env.call("nth", (3, value))?.into_rust::<String>()?;

        Ok(ReelRequest {
            url: reqwest::Url::parse(url.as_str())?,
            method: reqwest::Method::from_bytes(method.as_bytes())?,
            headers,
            body: reqwest::Body::from(body),
        })
    }
}

impl Into<reqwest::Request> for ReelRequest {
    fn into(self) -> reqwest::Request {
        let mut request = reqwest::Request::new(self.method, self.url);

        for (key, value) in self.headers {
            if let Some(k) = key {
                request.headers_mut().insert(k, value);
            }
        }

        request
    }
}

#[derive(Debug)]
pub struct ReelResponse {
    pub body: String,
    pub headers: Vec<(String, String)>,
    pub status_code: u8,
}

impl<'e> IntoLisp<'e> for ReelResponse {
    fn into_lisp(self, _env: &'e Env) -> EmacsResult<Value<'e>> {
        todo!()
    }
}

#[emacs::module(name = "reel-dyn")]
fn init(_env: &Env) -> EmacsResult<()> {
    // TODO initialize runtime
    println!("initializing");
    let runtime = tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()?;
    println!("runtime built");
    TOKIO_RUNTIME.set(runtime);
    Ok(())
}

#[emacs::defun]
fn execute_request(_env: &Env, request: ReelRequest) -> EmacsResult<ReelResponse> {
    let client = reqwest::Client::new();

    println!("executing request");

    TOKIO_RUNTIME
        .get()
        .unwrap()
        .spawn_blocking(move || client.execute(request.into()));

    todo!();
}

struct ListIterator<'e> {
    value: emacs::Value<'e>,
    index: usize,
    length: usize,
}

impl<'e> ListIterator<'e> {
    pub fn new(value: emacs::Value<'e>) -> EmacsResult<Self> {
        let length: usize = value.env.call("length", [value])?.into_rust()?;
        Ok(Self {
            value,
            length,
            index: 0,
        })
    }
}

impl<'e> Iterator for ListIterator<'e> {
    type Item = Value<'e>;

    fn next(&mut self) -> Option<Self::Item> {
        if self.index > self.length {
            return None;
        }

        self.index = self.index + 1;

        self.value.env.call("nth", (self.index, self.value)).ok()
    }
}

fn get_headers_from_alist(alist: Value<'_>) -> EmacsResult<HeaderMap> {
    if !alist.env.call("listp", [alist])?.is_not_nil() {
        return Err(emacs::Error::msg("Headers must be an alist"));
    }

    let list_iter = ListIterator::new(alist)?;

    let mut header_map = HeaderMap::new();

    for val in list_iter {
        println!("value: {:?}", val);
        let header: String = val.car::<Value>()?.into_rust()?;
        let value: String = val.cdr::<Value>()?.into_rust()?;

        header_map.insert(HeaderName::from_bytes(header.as_bytes())?, value.parse()?);
    }

    Ok(header_map)
}
