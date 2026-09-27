# TranslationUnit

The root of every ASR graph.

## Declaration

### Syntax

```text
TranslationUnit(symbol_table symtab, node* items, symbol? global_init,
    symbol? global_init_state, bool global_init_collective,
    symbol? global_init_bootstrap)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the global symbol table, with `id` 0. It owns every program, module, function and global variable of the translation unit. |
| `items` | statements and expressions that are not inside any program unit yet. Only the interactive frontends produce them; the `global_stmts` pass moves them into a program before the backends run, so a translation unit reaching a backend has an empty `items`. |
| `global_init` | the translation unit's startup initializer, a symbol of the global symbol table, or `nil`. It initializes state that belongs to no program unit: the companions of the saved coarrays of external procedures and of a program, the storage of variables declared directly in the translation unit, such as those of an interactive cell, and that of a module without an initializer of its own, such as one holding a COMMON block. It is a root of the startup engine like a module's, and is private to its object file. See [Program](../symbol_nodes/Program.md). |
| `global_init_state` | the saved `integer(4)` variable of the global symbol table that guards `global_init`, or `nil` exactly when `global_init` is. |
| `global_init_collective` | `true` when the initializer needs a collective boundary: it allocates a saved coarray, or depends on an initializer that does. Computed by the `coarray` pass; `false` without `global_init`. |
| `global_init_bootstrap` | the translation unit's collective bootstrap, `__lcompilers_collective_bootstrap` of the global symbol table, or `nil`: a private ordinary subroutine, not guarded, that starts the coarray runtime by calling `lcompilers_prif_start` and stops the program if that reports a failure. The `coarray` pass creates it whenever the translation unit calls the coarray runtime. At a collective boundary the engine runs it once per stable id, outside every guard, after the local initializers and before any collective one. |

### Return values

None.

## Description

A **TranslationUnit** is what a frontend produces and what every ASR pass and
backend consumes. It is the only constructor of the `unit` type, so it is also
the only thing an ASR text document may have at its root.

The global symbol table is the root of the symbol graph. Symbols nested deeper
(a variable of a program, a function of a module) live in the symbol table of
their owner, and every symbol table other than the global one is reachable from
it by walking the owning symbols.

## Examples

An ASR text document is always a **TranslationUnit**:

```{literalinclude} ../../examples/translationunit.asr
:language: clojure
```

## See Also

[Program](../symbol_nodes/Program.md), [Module](../symbol_nodes/Module.md), [Function](../symbol_nodes/Function.md)
