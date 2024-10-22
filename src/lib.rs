use emacs::{Env, FromLisp, IntoLisp, Result as EmacsResult};
use reqwest::header::{HeaderMap, HeaderName, HeaderValue};

emacs::plugin_is_GPL_compatible!();

#[derive(Debug)]
pub struct ConsCell<T, V>
where
    T: for<'t> FromLisp<'t>,
    V: for<'v> FromLisp<'v>,
{
    car: T,
    cdr: V,
}

impl<'e, T, V> FromLisp<'e> for ConsCell<T, V>
where
    T: for<'t> FromLisp<'t>,
    V: for<'v> FromLisp<'v>,
{
    fn from_lisp(value: emacs::Value<'e>) -> EmacsResult<Self> {
        let car = value.car()?;
        let cdr = value.cdr()?;

        Ok(ConsCell { car, cdr })
    }
}

impl<T, V> ConsCell<T, V>
where
    T: for<'t> emacs::FromLisp<'t>,
    V: for<'v> emacs::FromLisp<'v>,
{
    pub fn car(&self) -> &T {
        &self.car
    }

    pub fn cdr(&self) -> &V {
        &self.cdr
    }

    pub fn into_tuple(self) -> (T, V) {
        (self.car, self.cdr)
    }
}

impl<'e, T, V> From<ConsCell<T, V>> for (T, V)
where
    T: for<'t> emacs::FromLisp<'t>,
    V: for<'v> emacs::FromLisp<'v>,
{
    fn from(val: ConsCell<T, V>) -> Self {
        (val.car, val.cdr)
    }
}

#[derive(Debug)]
pub struct ReelResponse {
    pub body: String,
    pub headers: HeaderMap,
    pub status_code: u16,
}

#[derive(Debug)]
pub struct ReelClient {
    client: reqwest::blocking::Client,
}

#[derive(Debug)]
pub enum Body {
    String(String),
    Form(Vec<ConsCell<String, String>>),
}

impl<'e> FromLisp<'e> for Body {
    fn from_lisp(value: emacs::Value<'e>) -> EmacsResult<Self> {
        let list_value = value.env.call("proper-list-p", [value])?;

        if list_value.is_not_nil() {
            let mut vec = Vec::new();
            let env = value.env;

            let len = env.call("length", [value])?.into_rust::<usize>()?;

            for i in 0..len {
                let nth: ConsCell<String, String> = env.call("nth", (i, value))?.into_rust()?;
                vec.push(nth)
            }

            return Ok(Body::Form(vec));
        }

        let str_value = value.env.call("stringp", [value])?;

        if str_value.is_not_nil() {
            return Ok(Body::String(value.into_rust::<String>()?));
        }

        let received_type = value.env.call("type-of", [value])?;

        // this validation is done in lisp and this shouldn't occur
        // TODO refactor
        return value.env.signal(
            value.env.intern("reel-invalid-body-type")?,
            ("reel-valid-body-p", received_type),
        );
    }
}

#[emacs::module(name = "reel-dyn")]
fn init(env: &Env) -> EmacsResult<()> {
    env.call(
        "set",
        (
            env.intern("reel-dyn--version")?,
            option_env!("CARGO_PKG_VERSION"),
        ),
    )?;
    Ok(())
}

#[emacs::defun(user_ptr)]
fn make_client() -> EmacsResult<ReelClient> {
    Ok(ReelClient {
        client: reqwest::blocking::Client::builder()
            .use_rustls_tls()
            .build()?,
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
    body: Option<Body>,
) -> EmacsResult<ReelResponse> {
    let request = reel_client
        .client
        .request(reqwest::Method::from_bytes(method.as_bytes())?, url)
        .headers(headers.clone());

    let response = match body {
        Some(Body::String(string_body)) => request.body(string_body).send()?,
        Some(Body::Form(cells)) => {
            let pairs: Vec<(String, String)> = cells.into_iter().map(|cell| cell.into()).collect();

            request.form(&pairs).send()?
        }
        None => request.send()?,
    };

    let status_code = response.status();
    let headers = response.headers().clone();

    let body = response.text()?;

    Ok(ReelResponse {
        status_code: status_code.into(),
        headers,
        body,
    })
}
