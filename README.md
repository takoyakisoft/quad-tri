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
from "math"
from "greeter"

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
use "math"
use "greeter"

def main() -> int:
    greet("Tri modules")

    var sum: int := add(2, 5)
    prn("2 + 5 =")
    prn(sum)

    var fact: int := factorial(4)
    prn("4! =")
    prn(fact)

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
# stage0 driver (compiler front-end)
# - lex:   tokenize input
# - build: build an executable

# Lex (prints tokens)
cargo run -p qtri --release -- lex --lang quad ../examples/quad/hello.quad
cargo run -p qtri --release -- lex --lang tri  ../examples/tri/hello.tri

# Build & run a single-file example
cargo run -p qtri --release -- build --lang quad ../examples/quad/hello.quad -o target/tmp/quad_hello.exe
./target/tmp/quad_hello.exe

cargo run -p qtri --release -- build --lang tri  ../examples/tri/hello.tri -o target/tmp/tri_hello.exe
./target/tmp/tri_hello.exe

# Build & run module-based examples
cargo run -p qtri --release -- build --lang quad ../examples/quad/modules/main.quad -o target/tmp/quad_modules.exe
./target/tmp/quad_modules.exe

cargo run -p qtri --release -- build --lang tri  ../examples/tri/modules/main.tri -o target/tmp/tri_modules.exe
./target/tmp/tri_modules.exe

# Build & run struct examples (impl/imp with implicit self)
cargo run -p qtri --release -- build --lang quad ../examples/quad/structs.quad -o target/tmp/quad_structs.exe
./target/tmp/quad_structs.exe

cargo run -p qtri --release -- build --lang tri  ../examples/tri/structs.tri -o target/tmp/tri_structs.exe
./target/tmp/tri_structs.exe
```

## Roadmap toward self-hosting

See `spec/stage1.md` for the prioritized feature list (structs, dynamic arrays, etc.) needed before the Quad/Tri compiler can be built by Quad/Tri themselves.
