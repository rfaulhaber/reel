set shell := ['nu', '-c']

build env="dev":
    cargo build {{ if env == "release" { "--release" } else { "--workspace" } }}
    cp ./target/{{ if env == "release" { "release" } else { "debug" } }}/*reel.* .
    eask install

test: (build "dev")
    cargo build -p http_server
    pueued -d
    pueue add "cargo run -p http_server"
    eask test buttercup
    pueue kill --all

dist: (build "release")
    eask package

clean:
    cargo clean
    eask clean all
    rm libreel.*
