# TranslationUnit

The root of every ASR graph.

## Declaration

### Syntax

```text
TranslationUnit(symbol_table symtab, node* items)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the global symbol table, with `id` 0. It owns every program, module, function and global variable of the translation unit. |
| `items` | statements and expressions that are not inside any program unit yet. Only the interactive frontends produce them; the `global_stmts` pass moves them into a program before the backends run, so a translation unit reaching a backend has an empty `items`. |

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
