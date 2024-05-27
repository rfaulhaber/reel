use emacs::{Env, FromLisp, IntoLisp, Result as EmacsResult, Value};
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};

use std::str::FromStr;
use std::sync::OnceLock;
use std::{cell::OnceCell, collections::HashMap};

use tokio::runtime::{Builder, Runtime};

const TOKIO_RUNTIME: OnceCell<Runtime> = OnceCell::new();

mod client;
mod request;

emacs::plugin_is_GPL_compatible!();

#[derive(Debug)]
pub struct ReelRequest {
    pub url: String,
    pub method: reqwest::Method,
    pub headers: reqwest::header::HeaderMap,
    pub body: String,
}

impl<'e> IntoLisp<'e> for ReelRequest {
    fn into_lisp(self, env: &'e Env) -> EmacsResult<emacs::Value<'e>> {
        todo!()
    }
}

impl<'e> FromLisp<'e> for ReelRequest {
    fn from_lisp(value: emacs::Value<'e>) -> EmacsResult<Self> {
        let env = value.env;
        let url = env.call("nth", (0, value))?.into_rust::<String>()?;
        let method = env.call("nth", (1, value))?.into_rust::<String>()?;
        let headers_value: emacs::Value = env.call("nth", (2, value))?;
        let headers = get_headers(headers_value)?;
        let body = env.call("nth", (3, value))?.into_rust::<String>()?;
        todo!();
    }
}

#[emacs::module(name = "reel-dyn")]
fn init(env: &Env) -> EmacsResult<()> {
    // TODO initialize runtime
    let runtime = Builder::new_multi_thread().enable_all().build()?;
    TOKIO_RUNTIME.set(runtime);
    Ok(())
}

#[emacs::defun]
fn execute_request(env: &Env, request: ReelRequest) -> EmacsResult<emacs::Value<'_>> {
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

    let mut list_iter = ListIterator::new(alist)?;

    let mut header_map = HeaderMap::new();

    list_iter.try_for_each(|val| {
        let header: String = val.car::<Value>()?.into_rust()?;
        let value: String = val.cdr::<Value>()?.into_rust()?;

        header_map.insert(HeaderName::from_str(&header)?, value.parse()?);

        Ok(())
    });

    Ok(header_map)
}
