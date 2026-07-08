<p align="center">
    <img src="logo.png" width="100"/>
    <h1 align="center">Memphis</h1>
</p>

A Python interpreter and runtime written in Rust. It started as a learning exercise and is still experimental, but it now also powers companion tools for browser execution, bytecode inspection, and embeddable Python experiences. Think LLVM but exponentially less useful.

## Overview
Memphis contains a few execution modes, each for learning about a different aspect of interpreter/compiler development:
1. treewalk (default): farthest along in development.
1. bytecode VM: foundation complete, but missing many Python features.
 
See [SUPPORTED.md](docs/SUPPORTED.md) for details on specific features.

## Ecosystem
Memphis powers a small ecosystem of Python tools. Each run in the browser via WebAssembly.

- [Ozark](https://github.com/fromscratchcode/ozark): inspect Python from source to tokens, AST, and bytecode.
- [Tupelo](https://github.com/fromscratchcode/tupelo): run the Memphis REPL.
- [Shreve](https://github.com/fromscratchcode/shreve): run code in the playground or React embed.

## Design Goals
- Minimal dependencies. Uses zero dependencies by default. This means you can run Python code which does not call the stdlib (limiting, I know) through the treewalk interpreter or bytecode VM using no third-party Rust code. I find this kinda neat and worth preserving. The following crates are used behind feature flags for certain interfaces.
  - `wasm-bindgen`/`serde`: Only needed for WASM interface.
  - the `memphis-cli` crate uses:
    - `crossterm` for terminal REPL support
    - `clap` for CLI argument parsing
- No shortcuts. This is a learning exercise, so try to do things the "right" way, even if it takes a few tries.
- Functionality first, readability second, performance third. Don't do anything obviously wasteful, but there are dozens of non-optimal performance decisions in here in the name of a simple implementation.

## Installation
Installation requires a Rust toolchain with 2024 edition support.
```bash
git clone https://github.com/fromscratchcode/memphis
cd memphis
cargo install --path memphis-cli
```
## Usage
Use `memphis` as if it were `python`/`python3` and provide the path to a Python module.
```bash
memphis examples/async.py

# or run using the bytecode VM (WARNING: many features currently unsupported)
memphis --engine bytecode_vm examples/async.py
```
Or launch the REPL.
```bash
> memphis
memphis 0.1.0 REPL (engine: treewalk)
>>>
```
See [DEVELOPING.md](docs/DEVELOPING.md) for instructions on local development.

## Contributing
Memphis is a personal lab where I explore how Python works under the hood.

The code is open to explore and learn from, but I’m not actively seeking external contributions.

If you’re building something similar or just want to chat, feel free to reach out [on Discord](https://discord.com/invite/W3AEHyEh4G).

## Disclaimer
**Important Notice:** This project is currently in active development and is still considered experimental. As such, it is not recommended for use in production environments.

## License
This project is licensed under the MIT License. See `LICENSE` for details.
