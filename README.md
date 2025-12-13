# Quad/Tri (prototype)

Two indentation-based language prototypes.

- Quad: 4-letter keywords, strict typed (no type inference)
- Tri: 3-letter keywords, strict typed (no type inference)
- Assignment: `:=` (statement-only)
- Named args: `name: expr` (no mixing; duplicates are errors)
- Rust-style operator precedence
- Rust Stage0 compiler uses Cranelift backend

## Build
Requires: Rust

```bash
cd compiler
cargo build -p qtrt --release
cargo build -p qtri --release
```

## Run (for now)

```bash
cd compiler
cargo run -p qtri
```
