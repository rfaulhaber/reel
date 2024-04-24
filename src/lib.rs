use emacs::{Env, Result as EmacsResult};

use tokio::runtime::{Builder, Runtime};

mod client;
mod request;

emacs::plugin_is_GPL_compatible!();

#[emacs::module(name = "reel-dyn")]
fn init(env: &Env) -> EmacsResult<()> {
    Ok(())
}
