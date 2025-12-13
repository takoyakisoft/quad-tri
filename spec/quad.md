# Quad Syntax (Strict MVP)

## Keywords (4 letters)
from, type, enum, case, func, bind, cell, publ, priv, when, elif, else, loop, back, echo, stop, next, over

| Concept | Quad (4 letters) | Tri (3 letters) | Meaning |
| --- | --- | --- | --- |
| Public visibility | publ | pub | Public |
| Private visibility | priv | prv | Private |
| Immutable binding | bind | let | Immutable local variable |
| Mutable binding | cell | var | Mutable local variable |
| Continue | next | nxt | Loop continue |
| Character | char | chr | char / Unicode scalar value (u32) |
| Byte | byte | byt | u8 / unsigned char |

Stage1 reserved keywords (including unimplemented): impl, make, self, list, push, size,
open, read, save, shut, heap, free, okay, fail, trap

## Types
intg (i64), bool (i1), text (ptr to C string), void

- Fixed-length arrays use `[len]type` syntax and can nest (e.g. `[2][3]intg`).

## Structs
- Define with `type Name { field: type, ... }` using braces (no indent blocks).
- Each field may be prefixed with `publ` (public) or `priv` (private); omit for private.
- Create with `Name{ field: expr, ... }` providing every field exactly once.
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
- If the path has no extension, `.quad` is appended automatically.
- Paths are resolved relative to the importing file.