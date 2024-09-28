use emacs::{Env, IntoLisp, Result as EmacsResult};
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};

use tokio::runtime;

emacs::plugin_is_GPL_compatible!();

#[derive(Debug)]
pub struct ReelResponse {
    pub body: String,
    pub headers: HeaderMap,
    pub status_code: u16,
}

#[derive(Debug)]
pub struct ReelClient {
    client: reqwest::Client,
    runtime: runtime::Runtime,
}

#[emacs::module(name = "reel-dyn")]
fn init(_env: &Env) -> EmacsResult<()> {
    Ok(())
}

#[emacs::defun(user_ptr)]
fn make_client() -> EmacsResult<ReelClient> {
    Ok(ReelClient {
        client: reqwest::Client::new(),
        runtime: runtime::Builder::new_multi_thread()
            .enable_all()
            .build()
            .unwrap(),
    })
}

#[emacs::defun(user_ptr)]
fn make_header_map() -> EmacsResult<HeaderMap> {
    Ok(HeaderMap::new())
}

#[emacs::defun]
fn insert_header(map: &mut HeaderMap, key: String, value: String) -> EmacsResult<Option<String>> {
    let res = map.insert(
        HeaderName::from_bytes(key.as_bytes())?,
        HeaderValue::from_bytes(value.as_bytes())?,
    );

    match res {
        Some(v) => Ok(Some(v.to_str()?.to_string())),
        None => Ok(None),
    }
}

#[emacs::defun]
fn get_header(map: &HeaderMap, key: String) -> EmacsResult<Option<String>> {
    let value = map.get(HeaderName::from_bytes(key.as_bytes())?);

    match value {
        Some(v) => Ok(Some(v.to_str()?.to_string())),
        None => Ok(None),
    }
}

#[emacs::defun]
fn get_header_keys<'e, 'h>(env: &'e Env, map: &'h HeaderMap) -> EmacsResult<emacs::Value<'e>> {
    let mut keys = Vec::new();

    for key in map.keys() {
        keys.push(key.as_str().into_lisp(env)?)
    }

    env.call("list", &keys)
}

#[emacs::defun]
fn get_response_status(response: &ReelResponse) -> EmacsResult<u16> {
    Ok(response.status_code)
}

#[emacs::defun]
fn get_response_body(response: &ReelResponse) -> EmacsResult<String> {
    Ok(response.body.clone())
}

#[emacs::defun(user_ptr)]
fn get_response_headers(response: &ReelResponse) -> EmacsResult<HeaderMap> {
    Ok(response.headers.clone())
}

#[emacs::defun(user_ptr)]
fn make_client_request(
    reel_client: &ReelClient,
    url: String,
    method: String,
    headers: &HeaderMap,
    body: Option<String>,
) -> EmacsResult<ReelResponse> {
    let request = reel_client
        .client
        .request(reqwest::Method::from_bytes(method.as_bytes())?, url)
        .headers(headers.clone());

    let response = reel_client.runtime.block_on(async {
        // is there a less dumb way to do this
        if body.is_some() {
            request.body(body.unwrap()).send().await
        } else {
            request.send().await
        }
    })?;

    let status_code = response.status();
    let headers = response.headers().clone();

    let body = reel_client
        .runtime
        .block_on(async { response.text().await })?;

    Ok(ReelResponse {
        status_code: status_code.into(),
        headers,
        body,
    })
}

// #[emacs::defun(user_ptr)]
// fn get_response_status
