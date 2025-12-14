# Quad Syntax (Strict MVP)

## Keywords (4 letters)
from, type, enum, case, func, bind, cell, publ, priv, when, elif, else, loop, back, stop, next, over, impl, self, heap, free, trap

| Concept | Quad (4 letters) | Tri (3 letters) | Meaning |
| --- | --- | --- | --- |
| Public visibility | publ | pub | Public |
| Private visibility | priv | prv | Private |
| Immutable binding | bind | let | Immutable local variable |
| Mutable binding | cell | var | Mutable local variable |
| Continue | next | nxt | Loop continue |

Stage1 reserved keywords: (none)

## Types
int (i64), bool (i1), text (ptr to C string), void

- Text operators: `text + text -> text`, `text == text -> bool`, `text != text -> bool`.
- Ordering comparisons (`<`, `<=`, `>`, `>=`) are only defined for `int`.

- Fixed-length arrays use `[len]type` syntax and can nest (e.g. `[2][3]int`).

## Structs
- Define with `type Name:` followed by indented `field: type` lines.
- Each field may be prefixed with `publ` (public) or `priv` (private); omit for private.
- Create with `Name(field: expr, ...)` providing every field exactly once.
- Access fields with `expr.field`.
- Assignments may target fields: `foo.bar := expr`.
- Structs may be returned from functions.
- Enums use `enum Name:` with indented variants. Variants may carry payload types in parentheses
  and are matched with `case expr:` blocks using `when Enum::Variant(...)` patterns.

## Impl blocks
- Attach functions to a struct with `impl Name:` then indent one or more `func` definitions.
- The first parameter may be the keyword `self`, which is implicitly typed as `Name` and mutable
  inside the method. Pass it implicitly via `value.method(...)`.
- Functions inside `impl` without `self` act as associated functions and are called with
  `Name.method(...)`.

## Assignment
- `:=` only
- assignment is a statement (not an expression)

## Function calls
- Either all positional or all named (`name: expr`)
- No mixing
- Duplicate named args are compile errors

## Operator precedence (strong -> weak)
postfix: .  []  ()
unary:   !  -
mul:     * / %
add:     + -
shift:   << >>
bit:     &  ^  |
cmp:     < <= > >= == !=   (non-associative; chaining is an error)
logic:   &&  ||

## Blocks
- `:` then INDENT block
- INDENT/DEDENT are based on spaces only (tabs forbidden)

## Modules
- `from "path/to/file"` at the top level loads another Quad source file before the current one is type-checked.
- If the path has no extension, the compiler tries `.quad` first, then `.tri` (deterministic; no heuristics).
- Paths are resolved relative to the importing file.
- Standard library imports starting with `std/` resolve from the repository root.

## Pointers and heap allocation

Quad exposes a simple typed pointer form via `Addr<T>`.

- `heap(expr) -> Addr<T>` allocates a copy of `expr` on the heap.
- `deref(ptr) -> T` loads the value pointed to by `ptr`.
- `free(ptr)` deallocates the heap allocation.

Notes:

- `heap`, `deref`, and `free` are built-ins (and keywords), not user-defined functions.
- `free` is also used to drop managed runtime values like `text` and dynamic arrays.