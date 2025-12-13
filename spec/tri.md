# Tri Syntax (Strict MVP)

## Keywords (3 letters)
use, typ, def, let, var, iff, elf, els, for, ret, prn, brk, cnt, ovr

Stage1 で予約済みのキーワード（未実装のものを含む）: imp, new, slf, vec, psh, len, enm, cas, opn,
red, wrt, cls, mem, del, yep, nop, die

- use: import (Quad: from)
- typ: type/struct (Quad: type)
- def: function (Quad: func)
- let: immutable local (Quad: lock)
- var: mutable local (Quad: vars)
- iff/elf/els: if/elif/else (Quad: when/elif/else)
- for: loop (Quad: loop)
- ret: return (Quad: back)
- prn: print (Quad: echo)
- brk/cnt: break/continue (Quad: stop/next)
- ovr: foreach marker (Quad: over) [optional]

## Types (3 letters)
int (i64), bol (i1), txt (ptr to C string), vod (void)

## Structs
- Define with `typ Nam { fld: typ, ... }` using braces.
- Instantiate with `Nam{ fld: expr, ... }` and provide every field once.
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
- If the path has no extension, `.tri` is appended automatically.
- Paths are resolved relative to the importing file.
