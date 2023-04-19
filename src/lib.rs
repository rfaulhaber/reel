use emacs::{defun, Env, FromLisp, Result as EmacsResult, Value, Vector};
use std::collections::HashMap;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "reel-dyn")]
fn init(env: &Env) -> EmacsResult<()> {
    // This is run when Emacs loads the module.
    // More concretely, it is run after all the functions it defines are exported,
    // but before `(provide 'feature-name)` is (automatically) called.
    Ok(())
}

#[derive(Debug)]
struct ReelHeader {
    name: String,
    value: String,
}

impl ReelHeader {
    fn name(&self) -> &str {
        self.name.as_ref()
    }

    fn value(&self) -> &str {
        self.value.as_ref()
    }
}

#[derive(Debug)]
pub enum ReelMethod {
    GET,
    HEAD,
    POST,
    PUT,
    DELETE,
    CONNECT,
    OPTIONS,
    TRACE,
    PATCH,
}

#[derive(Debug)]
struct LispList<T>(Vec<T>)
where
    T: for<'a> FromLisp<'a>;

impl FromLisp<'_> for ReelHeader {
    fn from_lisp(value: Value<'_>) -> EmacsResult<Self> {
        let car = value.env.call("car", [value])?.into_rust()?;
        let cdr = value.env.call("cdr", [value])?.into_rust()?;

        Ok(ReelHeader {
            name: car,
            value: cdr,
        })
    }
}

impl<T: for<'a> emacs::FromLisp<'a>> FromLisp<'_> for LispList<T> {
    fn from_lisp(value: Value<'_>) -> EmacsResult<Self> {
        let vec_intern = value.env.intern("vector")?;
        let new_vec: Vector = value.env.call("apply", (vec_intern, value))?.into_rust()?;

        let mut lst = Vec::new();

        for e in new_vec {
            lst.push(e.into_rust()?);
        }

        Ok(LispList(lst))
    }
}

impl FromLisp<'_> for ReelMethod {
    fn from_lisp(value: Value<'_>) -> EmacsResult<Self> {
        let method_str: String = value.into_rust()?;

        match method_str.as_str() {
            "GET" => Ok(ReelMethod::GET),
            "HEAD" => Ok(ReelMethod::HEAD),
            "POST" => Ok(ReelMethod::POST),
            "PUT" => Ok(ReelMethod::PUT),
            "DELETE" => Ok(ReelMethod::DELETE),
            "CONNECT" => Ok(ReelMethod::CONNECT),
            "OPTIONS" => Ok(ReelMethod::OPTIONS),
            "TRACE" => Ok(ReelMethod::TRACE),
            "PATCH" => Ok(ReelMethod::PATCH),
            _ => Err(emacs::Error::msg("invalid HTTP method")),
        }
    }
}

#[defun]
fn execute_request(
    url: String,
    method: ReelMethod,
    headers: Option<LispList<ReelHeader>>,
    body: Option<String>,
) -> EmacsResult<()> {
    println!("args:");
    println!("url: {}", url);
    println!("method: {:?}", method);
    println!("headers: {:?}", headers);
    println!("body: {:?}", body);

    Ok(())
}

fn extract_headers(headers: Value) -> HashMap<String, String> {
    todo!();
}
