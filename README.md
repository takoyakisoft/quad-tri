# Quad/Tri (prototype)

Two indentation-based language prototypes.

- Quad: 4-letter keywords, strict typed (no type inference)
- Tri: 3-letter keywords, strict typed (no type inference)
- Assignment: `:=` (statement-only)
- Named args: `name: expr` (no mixing; duplicates are errors)
- Rust-style operator precedence
- Rust Stage0 compiler uses Cranelift backend
- **New**: Struct methods (`impl`/`imp` blocks) with `self`/`slf` receiver
- **New**: Module imports (`from`/`use`) with automatic `.quad`/`.tri` extension resolution
- **New**: Enums with variant patterns via `case`/`cas` and `Enum::Variant` matching
- Fixed-length arrays with `[len]type` syntax, array literals, and nested indexing

| Concept | Quad (4 letters) | Tri (3 letters) | Meaning |
| --- | --- | --- | --- |
| Public visibility | publ | pub | Public |
| Private visibility | priv | prv | Private |
| Immutable binding | bind | let | Immutable local variable |
| Mutable binding | cell | var | Mutable local variable |
| Continue | next | nxt | Loop continue |

## Examples

Quad uses four-letter keywords and `from` imports. A minimal module-based program (see `examples/quad/modules`):

```text
from "math"
from "greeter"

func main() -> int:
    greet("Quad modules")

    cell sum: int := add(2, 5)
    println("2 + 5 =")
    println(sum)

    cell fact: int := factorial(4)
    println("4! =")
    println(fact)

    back 0
```

Tri mirrors the same program with three-letter keywords and `use` imports (see `examples/tri/modules`):

```text
use "math"
use "greeter"

def main() -> int:
    greet("Tri modules")

    var sum: int := add(2, 5)
    println("2 + 5 =")
    println(sum)

    var fact: int := factorial(4)
    println("4! =")
    println(fact)

    ret 0
```

Enums and pattern matching are available with `enum`/`enm` declarations and `case`/`cas` blocks:

```text
enum Event:
    Quit
    Click(int, int)
    Key(text)

func handle(e: Event) -> void:
    case e:
        when Event::Quit:
            println("Bye")

        when Event::Click(x, y):
            println("Clicked at:")
            println(x)

        when Event::Key(k):
            println("Key pressed:")
            println(k)
```

```text
enm Evt:
    Qit
    Clk(int, int)
    Key(text)

def hnd(e: Evt) -> void:
    cas e:
        iff Evt::Qit:
            println("Bye")

        iff Evt::Clk(x, y):
            println("Clk")
            println(x)

        iff Evt::Key(k):
            println("Key")
            println(k)
```

## Build
Requires: Rust

```bash
cargo build -p qtrt --release
cargo build -p qtri --release
```

## Run (for now)

```bash
# stage0 driver (compiler front-end)
# - lex:   tokenize input
# - build: build an executable

# Lex (prints tokens)
cargo run -p qtri --release -- lex --lang quad examples/quad/hello.quad
cargo run -p qtri --release -- lex --lang tri  examples/tri/hello.tri

# Build & run a single-file example
cargo run -p qtri --release -- build --lang quad examples/quad/hello.quad -o target/tmp/quad_hello.exe
./target/tmp/quad_hello.exe

cargo run -p qtri --release -- build --lang tri  examples/tri/hello.tri -o target/tmp/tri_hello.exe
./target/tmp/tri_hello.exe

# Build & run module-based examples
cargo run -p qtri --release -- build --lang quad examples/quad/modules/main.quad -o target/tmp/quad_modules.exe
./target/tmp/quad_modules.exe

cargo run -p qtri --release -- build --lang tri  examples/tri/modules/main.tri -o target/tmp/tri_modules.exe
./target/tmp/tri_modules.exe

# Build & run struct examples (impl/imp with implicit self)
cargo run -p qtri --release -- build --lang quad examples/quad/structs.quad -o target/tmp/quad_structs.exe
./target/tmp/quad_structs.exe

cargo run -p qtri --release -- build --lang tri  examples/tri/structs.tri -o target/tmp/tri_structs.exe
./target/tmp/tri_structs.exe
```

## Roadmap toward self-hosting

See `spec/stage1.md` for the prioritized feature list (structs, dynamic arrays, etc.) needed before the Quad/Tri compiler can be built by Quad/Tri themselves.
