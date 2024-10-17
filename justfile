set shell := ['nu', '-c']

build env="dev":
    cargo build {{ if env == "release" { "--release" } else { "" } }}
    cp ./target/{{ if env == "release" { "release" } else { "debug" } }}/libreel.* .
    eask install

test: (build "dev")
    just test-setup
    -eask test buttercup
    just test-teardown

dist: (build "release")
    eask package

clean:
    cargo clean
    rm libreel.*

[private]
test-setup:
    #!/usr/bin/env nu
    print "preparing test environment"
    ^docker run -d --rm --name reel-test -p 8080:80 kennethreitz/httpbin

    mut state = (^docker inspect reel-test | from json | first | get State.status)

    print $"state: ($state)"

    while $state != "running" {
        print "waiting..."
        $state = (^docker inspect reel-test | from json | first | get State.status)
    }

    sleep 1sec

[private]
test-teardown:
    #!/usr/bin/env nu
    print "tearing down"
    ^docker stop reel-test
