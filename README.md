# Quad/Tri (prototype)

Two indentation-based language prototypes.

- Quad: 4-letter keywords, strict typed (no type inference)
- Tri: 3-letter keywords, strict typed (no type inference)
- Assignment: `:=` (statement-only)
- Named args: `name: expr` (no mixing; duplicates are errors)
- Rust-style operator precedence
- Rust Stage0 compiler uses Cranelift backend

## Examples

Quad uses four-letter keywords and `from` imports. A minimal module-based program (see `examples/quad/modules`):

```quad
from "math.quad"
from "greeter.quad"

func main() -> intg:
    greet("Quad modules")

    vars sum: intg := add(2, 5)
    echo("2 + 5 =")
    echo(sum)

    vars fact: intg := factorial(4)
    echo("4! =")
    echo(fact)

    back 0
```

Tri mirrors the same program with three-letter keywords and `use` imports (see `examples/tri/modules`):

```tri
use "math.tri"
use "greeter.tri"

def main() -> int:
    greet("Tri modules")

    var sum: int := add(3, 4)
    prn("3 + 4 =")
    prn(sum)

    var fact_val: int := fact(5)
    prn("5! =")
    prn(fact_val)

    ret 0
```

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
