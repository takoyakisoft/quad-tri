This repository was entirely created by AI.
Therefore, it is recommended to read it through AI.
Gemini 3.0 Pro created most of the code, and GPT-5.2 handled bug fixing.

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
| Import | from | use | Import module |
| Function | func | def | Function definition |
| Structure | type | typ | Struct definition |
| Implementation | impl | imp | Method implementation |
| Enumeration | enum | enm | Enum definition |
| Case | case | cas | Pattern matching block |
| Public visibility | publ | pub | Public modifier |
| Private visibility | priv | prv | Private modifier |
| Immutable binding | bind | let | Immutable local variable |
| Mutable binding | cell | var | Mutable local variable |
| If | when | iff | Conditional |
| Else If | elif | elf | Conditional else-if |
| Else | else | els | Conditional else |
| Loop | loop | for | Loop structure |
| Return | back | ret | Return from function |
| Break | stop | brk | Break loop |
| Continue | next | nxt | Continue loop |

## Memory and pointers

Quad/Tri heap allocation uses `std/mem` functions `alloc(expr)` and `dealloc(ptr)`.
Pointer types are `Addr<T>` (Quad) and `Ptr<T>` (Tri).

## Standard library (prototype)

The repository ships a small prototype standard library under `std/`.

- `std/vec`: resizable vectors (implemented via raw pointer intrinsics)
- `std/map`: a minimal `text -> int` map built on `std/vec`
- `std/io`: `print`/`println` wrappers
- `std/conv`: `atoi`/`itoa` wrappers
- `std/os`: `args()` and process helpers
- `std/mem`: explicit allocation helpers (renamed from the previous `std/alloc` module)

## Examples

### Hello World

Quad:

```gdscript
func main() -> int:
    println("Hello, World!")
    back 0
```

Tri:

```python
def main() -> int:
    println("Hello, World!")
    ret 0
```

### FizzBuzz

Quad (prints 1..=20):

```gdscript
func main() -> int:
    cell i: int := 1
    loop i <= 20:
        when i % 15 == 0:
            println("FizzBuzz")
        elif i % 3 == 0:
            println("Fizz")
        elif i % 5 == 0:
            println("Buzz")
        else:
            println(i)
        i := i + 1
    back 0
```

Tri (prints 1..=20):

```python
def main() -> int:
    var i: int := 1
    for i <= 20:
        iff i % 15 == 0:
            println("FizzBuzz")
        elf i % 3 == 0:
            println("Fizz")
        elf i % 5 == 0:
            println("Buzz")
        els:
            println(i)
        i := i + 1
    ret 0
```

### Practical example: arrays + dictionary + structs + impl

This example combines:

- Fixed-length arrays (`[len]T`)
- A small dictionary (std `StrMap`: `text -> int`)
- A user-defined struct with methods (`impl`/`imp`)

Quad:

```gdscript
from "std/map"

type WordCounter:
    publ m: StrMap

impl WordCounter:
    func make() -> WordCounter:
        back WordCounter(m: StrMap.make())

    func add(self, w: text) -> void:
        when self.m.contains(w):
            self.m.insert(w, self.m.get(w) + 1)
        else:
            self.m.insert(w, 1)

    func drop(self) -> void:
        self.m.drop()

func main() -> int:
    cell words: [6]text := [
        "foo",
        "bar",
        "foo",
        "baz",
        "foo",
        "bar"
    ]

    cell c: WordCounter := WordCounter.make()

    cell i: int := 0
    loop i < 6:
        c.add(words[i])
        i := i + 1

    println("foo:")
    println(c.m.get("foo"))
    println("bar:")
    println(c.m.get("bar"))

    c.drop()
    back 0
```

Tri:

```python
use "std/map"

typ Wrd:
    pub m: StrMap

imp Wrd:
    def mak() -> Wrd:
        ret Wrd(m: StrMap.make())

    def add(slf, w: text) -> void:
        iff slf.m.contains(w):
            slf.m.insert(w, slf.m.get(w) + 1)
        els:
            slf.m.insert(w, 1)

    def drp(slf) -> void:
        slf.m.drop()

def main() -> int:
    var wds: [6]text := [
        "foo",
        "bar",
        "foo",
        "baz",
        "foo",
        "bar"
    ]

    var c: Wrd := Wrd.mak()

    var i: int := 0
    for i < 6:
        c.add(wds[i])
        i := i + 1

    println("foo:")
    println(c.m.get("foo"))
    println("bar:")
    println(c.m.get("bar"))

    c.drp()
    ret 0
```

Quad uses four-letter keywords and `from` imports. A minimal module-based program (see `examples/quad/modules`):

```gdscript
from "std/math"
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

```python
use "std/math"
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

```gdscript
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

```python
enm Event:
    Quit
    Click(int, int)
    Key(text)

def handle(e: Event) -> void:
    cas e:
        iff Event::Quit:
            println("Bye")

        iff Event::Click(x, y):
            println("Clicked at:")
            println(x)

        iff Event::Key(k):
            println("Key pressed:")
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
cargo run -p qtri --release -- lex --lang quad examples/readme/hello.quad
cargo run -p qtri --release -- lex --lang tri  examples/readme/hello.tri

# Build & run a single-file example
cargo run -p qtri --release -- build --lang quad examples/readme/hello.quad -o target/tmp/quad_hello.exe
./target/tmp/quad_hello.exe

cargo run -p qtri --release -- build --lang tri  examples/readme/hello.tri -o target/tmp/tri_hello.exe
./target/tmp/tri_hello.exe

# Build & run module-based examples
cargo run -p qtri --release -- build --lang quad examples/readme/modules/main.quad -o target/tmp/quad_modules.exe
./target/tmp/quad_modules.exe

cargo run -p qtri --release -- build --lang tri  examples/readme/modules/main.tri -o target/tmp/tri_modules.exe
./target/tmp/tri_modules.exe

# Build & run struct examples (impl/imp with implicit self)
cargo run -p qtri --release -- build --lang quad examples/readme/word_counter.quad -o target/tmp/quad_structs.exe
./target/tmp/quad_structs.exe

cargo run -p qtri --release -- build --lang tri  examples/readme/word_counter.tri -o target/tmp/tri_structs.exe
./target/tmp/tri_structs.exe
```

## Roadmap toward self-hosting

See `spec/stage1.md` for the prioritized feature list (structs, dynamic arrays, etc.) needed before the Quad/Tri compiler can be built by Quad/Tri themselves.
