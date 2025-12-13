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
| Character | char | chr | char / Unicode scalar value (u32) |
| Byte | byte | byt | u8 / unsigned char |

## Examples

Quad uses four-letter keywords and `from` imports. A minimal module-based program (see `examples/quad/modules`):

```gdscript
from "math"
from "greeter"

func main() -> intg:
    greet("Quad modules")

    cell sum: intg := add(2, 5)
    echo("2 + 5 =")
    echo(sum)

    cell fact: intg := factorial(4)
    echo("4! =")
    echo(fact)

    back 0
```

Tri mirrors the same program with three-letter keywords and `use` imports (see `examples/tri/modules`):

```python
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

Enums and pattern matching are available with `enum`/`enm` declarations and `case`/`cas` blocks:

```gdscript
enum Event:
    Quit
    Click(intg, intg)
    Key(text)

func handle(e: Event) -> void:
    case e:
        when Event::Quit:
            echo("Bye")

        when Event::Click(x, y):
            echo("Clicked at:")
            echo(x)

        when Event::Key(k):
            echo("Key pressed:")
            echo(k)
```

```python
enm Evt:
    Qit
    Clk(int, int)
    Key(txt)

def hnd(e: Evt) -> vod:
    cas e:
        iff Evt::Qit:
            prn("Bye")

        iff Evt::Clk(x, y):
            prn("Clk")
            prn(x)

        iff Evt::Key(k):
            prn("Key")
            prn(k)
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
