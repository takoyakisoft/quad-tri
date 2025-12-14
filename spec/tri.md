# Tri Syntax (Strict MVP)

## Keywords (3 letters)
use, typ, enm, cas, def, let, var, pub, prv, iff, elf, els, for, ret, brk, nxt, ovr, imp, slf, mem, del, die

| Concept | Quad (4 letters) | Tri (3 letters) | Meaning |
| --- | --- | --- | --- |
| Public visibility | publ | pub | Public |
| Private visibility | priv | prv | Private |
| Immutable binding | bind | let | Immutable local variable |
| Mutable binding | cell | var | Mutable local variable |
| Continue | next | nxt | Loop continue |

Stage1 reserved keywords: (none)

- use: import (Quad: from)
- typ: type/struct (Quad: type)
- def: function (Quad: func)
- let: immutable local (Quad: bind)
- var: mutable local (Quad: cell)
- iff/elf/els: if/elif/else (Quad: when/elif/else)
- for: loop (Quad: loop)
- ret: return (Quad: back)
- brk/nxt: break/continue (Quad: stop/next)
- ovr: foreach marker (Quad: over) [optional]

## Enums and pattern matching
- Declare with `enm Name:` followed by indented variants. Payload types appear in parentheses.
- Match with `cas expr:` and variant patterns like `iff Name::Variant(x, y):` within the block.

## Types
Types are identifiers (not reserved words). Canonical spellings are: int (i64), bool (i1), text (ptr to C string), void.

- Text operators: `text + text -> text`, `text == text -> bool`, `text != text -> bool`.
- Ordering comparisons (`<`, `<=`, `>`, `>=`) are only defined for `int`.

- Fixed-length arrays use `[len]type` syntax and can nest (e.g. `[2][3]int`).

## Structs
- Define with `typ Nam:` followed by indented `fld: typ` lines.
- Each field may be prefixed with `pub` (public) or `prv` (private); omit for private.
- Instantiate with `Nam(fld: expr, ...)` and provide every field once.
- Access fields via `expr.fld` and assign with `foo.fld := expr`.
- Structs may be returned from functions.

## Impl blocks
- Attach functions to a struct with `imp Nam:` then indent one or more `def` definitions.
- The first parameter may be the keyword `slf`, which is implicitly typed as the struct name and
  mutable inside the method. Pass it implicitly via `value.method(...)`.
- Functions inside `imp` without `slf` act as associated functions and are called with
  `Nam.method(...)`.

## Assignment
- `:=` only (statement-only)

## Function calls
- Either all positional or all named (`name: expr`)
- No mixing / duplicate names are errors

## Operator precedence
(same as Quad)

## Modules
- `use "path/to/file"` at the top level loads another Tri source file before the current one is type-checked.
- If the path has no extension, the compiler tries `.tri` first, then `.quad` (deterministic; no heuristics).
- Paths are resolved relative to the importing file.
- Standard library imports starting with `std/` resolve from the repository root.

## Pointers and heap allocation

Tri exposes a simple typed pointer form via `Ptr<T>`.

- `alloc(expr) -> Ptr<T>` allocates a copy of `expr` on the heap.
- `dealloc(ptr)` deallocates the heap allocation.

Notes:

- `mem` and `del` are built-ins (and keywords), not user-defined functions.
