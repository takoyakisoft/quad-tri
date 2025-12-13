# Agent Instructions

## Scope
These instructions apply to the entire repository.

## Language conventions
- Follow the language reference in `spec/quad.md` and `spec/tri.md` when editing parser, lexer, semantics, or examples. Keep keywords, operator precedence, and type names aligned with these specs.
- Assignment is statement-only via `:=`; do not introduce expression assignments.
- Function calls must be either fully positional or fully named (`name: expr`) with no mixing or duplicate names.
- Blocks are introduced with `:` and indented using spaces only (tabs are forbidden).

## Development notes
- Build and test using Cargo from the `compiler` directory (`cargo build -p qtrt --release`, `cargo build -p qtri --release`, and `cargo check/test`). The compiler uses the Cranelift backend, so LLVM is not required, but a system linker (MSVC link.exe or cc) is needed.
- When adding examples or docs, keep keyword spellings consistent: Quad uses 4-letter keywords; Tri uses 3-letter keywords.
- **Modules**: Import paths without extensions (e.g., `from "math"`) automatically map to `.quad` or `.tri` files. Imports must appear at the top of the file.
- **Structs & Methods**: Use `impl` (Quad) or `imp` (Tri) blocks to define methods. The first parameter `self` (Quad) or `slf` (Tri) is required for methods (receivers) but omitted for associated functions.
