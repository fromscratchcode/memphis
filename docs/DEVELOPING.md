# Developing
## Local Development
This project will live and die by `cargo`.
```bash
cargo build
cargo test
cargo run examples/test.py
```
## REPL
The REPL is useful for interactive mode. It is currently gated behind a feature flag because it pulls in crossterm.
```bash
cargo run --features repl
```
## Feature Flags
Feature flags are needed to enable REPL support or the WASM interface.
```bash
# script to run all combinations of feature flags
./test_features.sh
```
## Benchmarking
To compare runtime, we can build in release mode and use the different engines.
```bash
cargo install --path . --all-features
hyperfine "memphis examples/loop_perf.py" "MEMPHIS_ENGINE=bytecode_vm memphis examples/loop_perf.py" --warmup 5
```
### Flamegraph
This is a cool way to visualize why a bytecode VM is more performant than a treewalk interpreter.
```bash
cargo install flamegraph
cargo build --all-features
# we require debug symbols to produce a flamegraph, hence invoking the binary from `target/debug`.
sudo flamegraph -v -o tw.svg -- target/debug/memphis examples/loop_perf.py
sudo flamegraph -v -o vm.svg -- MEMPHIS_ENGINE=bytecode_vm target/debug/memphis examples/loop_perf.py
```

## WebAssembly
```bash
# wasm-pack helps compile our Rust code to WebAssembly and bundle it with JavaScript bindings we
# can call from an HTML/JavaScript page.
cargo install wasm-pack

# wasm-pack also downloads the wasm32-unknown-unknown target via rustup for us.
# We must specify a feature flag because our wasm_bindgen interface is behind the wasm feature flag.
wasm-pack build --target web -- --features wasm
```
