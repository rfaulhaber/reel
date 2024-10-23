use std::collections::HashMap;

use axum::{extract::Request, http::{HeaderMap, HeaderValue}, routing::any, Router};
use serde::Serialize;
use http_body_util::BodyExt;

type QueryParameters<'qp> = HashMap<&'qp str, Option<&'qp str>>;

#[derive(Serialize)]
struct Response<'r> {
    query_parameters: Option<QueryParameters<'r>>,
    headers: HashMap<&'r str, &'r str>,
    body: &'r str,
    url: &'r str,
    method: &'r str,
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/anything", any(anything))
        .route("/anything/*rest", any(anything))
        .route("/status", any(status));

    let listener = tokio::net::TcpListener::bind("0.0.0.0:8080").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

async fn anything(request: Request) -> String {
    let (parts, body) = request.into_parts();

    let headers = extract_headers(&parts.headers);

    let query = parts.uri.query();

    let query_parameters = query.map(extract_query_parameters);

    let method = parts.method.to_string();
    let method = method.as_str();
    let uri = parts.uri.to_string();
    let url = uri.as_str();


    let bytes = body.collect().await.unwrap().to_bytes();

    let resp = Response {
        query_parameters,
        headers,
        method,
        url,
        body: std::str::from_utf8(&bytes).unwrap(),
    };

    serde_json::to_string(&resp).unwrap()
}

async fn status(request: Request) -> String {
    todo!()
}

fn extract_query_parameters<'p>(query: &'p str) -> QueryParameters<'p> {
    let values: Vec<(&str, &str)> = query
        .split("&")
        .map(|pair| {
            let split: Vec<&str> = pair.split("=").collect();
            (split[0], split[1])
        })
        .collect();

    let mut map = HashMap::new();

    for (key, value) in values {
        map.insert(key, if value == "" { None } else { Some(value) });
    }

    map
}

fn extract_headers<'h>(headers: &'h HeaderMap<HeaderValue>) -> HashMap<&'h str, &'h str> {
    let mut map = HashMap::new();

    for header in headers.keys() {
        map.insert(header.as_str(), headers.get(header).unwrap().to_str().unwrap());
    }

    map
}
