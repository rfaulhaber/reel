use emacs::{defun, Env, FromLisp, IntoLisp, Result as EmacsResult, Value, Vector};
use hyper::{
    body::{Body, Bytes},
    http::{uri::Scheme, HeaderName, HeaderValue},
};
use once_cell::sync::Lazy;
use std::{collections::HashMap, marker::PhantomData, sync::Arc};
use tokio::{
    net::TcpStream,
    runtime::{Builder, Runtime},
};

use http_body_util::BodyExt;

static RUNTIME: Lazy<Arc<Runtime>> =
    Lazy::new(|| Arc::new(Builder::new_multi_thread().enable_all().build().unwrap()));

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

struct ReelRequest<'r> {
    url: &'r str,
    method: ReelMethod,
    headers: Vec<ReelHeader>,
    body: Option<&'r str>,
}

impl<'e> FromLisp<'e> for ReelRequest<'_> {
    fn from_lisp(value: Value<'e>) -> EmacsResult<Self> {
        todo!()
    }
}

struct ReelResponse<'r> {
    status: u16,
    body: &'r str,
    headers: HashMap<String, String>,
}

impl<'e, 'r> IntoLisp<'e> for ReelResponse<'r> {
    fn into_lisp(self, env: &'e Env) -> EmacsResult<Value<'_>> {
        let mut cons_cells = Vec::with_capacity(self.headers.len());

        for (key, value) in self.headers.iter() {
            cons_cells.push(env.cons(key, value)?);
        }

        let list = env.list(&cons_cells)?;

        let vec = env.vector((self.status, String::from(self.body), list))?;

        Ok(vec)
    }
}

// impl<'r, T> From<hyper::Response<T>> for ReelResponse<'r> {
//     fn from(value: hyper::Response<T>) -> Self {
//         ReelResponse {
//             status: value.status().as_u16(),
//             body: value.body(),
//             headers: (),
//         }
//     }
// }

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

    let mut hyper_req = hyper::Request::builder()
        .uri(url.clone())
        .header(hyper::header::USER_AGENT, "GNU Emacs/reel")
        .header(
            hyper::header::HOST,
            url.parse::<hyper::Uri>()?
                .authority()
                .unwrap()
                .clone()
                .as_str(),
        )
        .method(match method {
            ReelMethod::GET => hyper::Method::GET,
            ReelMethod::HEAD => hyper::Method::HEAD,
            ReelMethod::POST => hyper::Method::POST,
            ReelMethod::PUT => hyper::Method::PUT,
            ReelMethod::DELETE => hyper::Method::DELETE,
            ReelMethod::CONNECT => hyper::Method::CONNECT,
            ReelMethod::OPTIONS => hyper::Method::OPTIONS,
            ReelMethod::TRACE => hyper::Method::TRACE,
            ReelMethod::PATCH => hyper::Method::PATCH,
        })
        .body(body.unwrap_or("".to_string()))?;

    if let Some(header_values) = headers {
        for header in header_values.0 {
            let header_name = HeaderName::from_bytes(header.name().to_lowercase().as_bytes())?;
            let header_value = HeaderValue::from_bytes(header.value().to_lowercase().as_bytes())?;

            hyper_req.headers_mut().append(header_name, header_value);
        }
    }

    if !hyper_req.headers().contains_key(hyper::header::ACCEPT) {
        hyper_req.headers_mut().append(
            hyper::header::ACCEPT,
            HeaderValue::from_bytes("*/*".as_bytes())?,
        );
    }

    println!("request: {:?}", hyper_req);

    let res = RUNTIME.block_on(make_request(hyper_req))?;

    Ok(())
}

async fn make_request<T>(req: hyper::Request<T>) -> EmacsResult<()>
where
    T: Send + Sync + Body + 'static,
    <T as Body>::Data: Send,
    <T as Body>::Error: std::error::Error,
    <T as Body>::Error: Send,
    <T as Body>::Error: Sync,
{
    println!("making request");
    // Get the host and the port
    let host = req.uri().host().expect("uri has no host");
    let scheme = req.uri().scheme_str();
    let port = req.uri().port_u16().unwrap_or_else(|| match scheme {
        Some("http") | None => 80,
        Some("https") => 443,
        _ => todo!(),
    });

    let address = format!("{}:{}", host, port);

    // Open a TCP connection to the remote host
    let stream = TcpStream::connect(address).await?;

    // Perform a TCP handshake
    let (mut sender, conn) = hyper::client::conn::http1::handshake(stream).await?;

    // Spawn a task to poll the connection, driving the HTTP state
    tokio::task::spawn(async move {
        if let Err(err) = conn.await {
            println!("Connection failed: {:?}", err);
        }
    });

    let mut res = sender.send_request(req).await?;

    println!("Response: {}", res.status());
    println!("Headers: {:#?}\n", res.headers());
    println!("Body: {:#?}\n", res.body());

    let body_res = res.collect().await?;

    println!("collected body: {:?}", body_res.to_bytes());

    Ok(())
}
