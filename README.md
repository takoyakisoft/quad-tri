# Quad/Tri (prototype)

Two indentation-based language prototypes.

- Quad: 4-letter keywords, strict typed (no type inference)
- Tri: 3-letter keywords, strict typed (no type inference)
- Assignment: `:=` (statement-only)
- Named args: `name: expr` (no mixing; duplicates are errors)
- Rust-style operator precedence
- Rust Stage0 compiler emits LLVM IR

## Build
Requires: Rust + LLVM tools (`llc`)

```bash
cd compiler
cargo build -p quadrt --release
cargo build -p quad0 --release
```

## Run (for now)

```bash
cd compiler
cargo run -p quad0
```
