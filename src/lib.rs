use emacs::{Env, Result as EmacsResult};

use std::cell::OnceCell;
use std::sync::OnceLock;

use tokio::runtime::{Builder, Runtime};

mod client;
mod request;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "reel-dyn")]
fn init(env: &Env) -> EmacsResult<()> {
    // TODO initialize runtime
    Ok(())
}
