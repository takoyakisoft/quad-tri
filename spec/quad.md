# Quad Syntax (Strict MVP)

## Keywords (4 letters)
from, type, func, lock, vars, when, elif, else, loop, back, echo, stop, next, over

## Types
intg (i64), bool (i1), text (ptr to C string), void

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