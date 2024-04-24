use std::collections::HashMap;

use emacs::{defun, Env, FromLisp, Result as EmacsResult, Value, Vector};

const REEL_USER_AGENT: &'static str = "GNU Emacs/reel";

#[derive(Debug)]
pub struct ClientConfig<'config> {
    user_agent: Option<&'config str>,
    use_cookies: Option<bool>,
    default_headers: Option<HashMap<String, String>>,
    timeout: Option<u32>,
    connect_timeout: Option<u32>,
}

#[derive(Debug)]
pub struct Client {
    client: reqwest::Client,
}

impl Client {
    pub fn new(config: ClientConfig) -> Client {
        let builder = reqwest::Client::builder().user_agent(REEL_USER_AGENT);

        Client {
            client: builder.build().unwrap(), // TODO handle result
        }
    }
}

#[defun(user_ptr)]
pub fn make_client(
    user_agent: Option<String>,
    use_cookies: Option<bool>,
    default_headers: Option<bool>,
    timeout: u32,
    connect_timeout: u32,
) -> EmacsResult<Client> {
    todo!();
}
