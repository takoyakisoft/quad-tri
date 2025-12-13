# Tri Syntax (Strict MVP)

## Keywords (3 letters)
use, typ, def, let, var, iff, elf, els, for, ret, prn, brk, cnt, ovr

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

## Assignment
- `:=` only (statement-only)

## Function calls
- Either all positional or all named (`name: expr`)
- No mixing / duplicate names are errors

## Operator precedence
(same as Quad)

## Modules
- `use "path/to/file.tri"` at the top level loads another Tri source file before the current one is type-checked.
- Paths are resolved relative to the importing file.
