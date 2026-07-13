#!/usr/bin/env bash
set -euo pipefail

# Build the wasm package and force an empty pkg/.npmignore so npm packs pkg/*
# instead of inheriting pkg/.gitignore, which would exclude the generated files.
wasm-pack build ../memphis --target web --out-dir ../memphis-js/pkg -- --features wasm
: > pkg/.npmignore
