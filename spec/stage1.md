# Stage1 self-hosting prerequisites

To move the Quad/Tri compiler toward Stage1 (compiled by Quad/Tri themselves), we need more data-structure and I/O support beyond the current arithmetic/functions/variables/loops baseline. A compiler is a large data-processing program, so the following capabilities are essential (ordered by priority):

1. **Struct types** (`type` / `typ`)
   - Without structs we cannot bundle tokens (`Token`) or syntax nodes (`Expr`, `Stmt`), leaving related fields scattered across separate variables.
2. **Dynamic arrays**
   - Source text is a list of characters, tokens form a list, and function bodies are lists of statements; resizable arrays are mandatory.
3. **Sum types / tagged unions**
   - AST nodes branch between forms like `If` / `While` / assignment; a discriminated union is the clean way to express this.
4. **String manipulation**
   - Current `text` acts as a raw `char*`; slicing, concatenation, and per-character inspection are needed for lexing and parsing.
5. **File I/O**
   - Loading source files requires more than `println`; we need reads in addition to writes.
6. **Memory management**
   - Structs and dynamic arrays require heap allocation.
   - Quad uses `heap`/`free` (and `deref`) while Tri uses `mem`/`del`.
   - The current prototype runtime/std also exposes low-level intrinsics like `alloc_bytes` and `sys_dealloc`.

**Next-step suggestion:** Implement struct definitions (`type` / `typ`) first. They are the foundation for building `Result`/`Option`-like types and organizing compiler data during self-hosting.
