# ExternalSymbol

A symbol that lives in another scope, made visible here.

## Declaration

### Syntax

```text
ExternalSymbol(symbol_table parent_symtab, identifier name,
    symbol external, identifier module_name, identifier* scope_names,
    identifier original_name, access access)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table this symbol is stored in. |
| `name` | the name the symbol is known by here, after any renaming. |
| `external` | the symbol this one stands for. |
| `module_name` | the name of the module the original symbol is declared in. |
| `scope_names` | the names of the scopes to walk inside the module to reach the symbol, outermost first. Empty when the symbol is declared directly in the module. |
| `original_name` | the name the symbol has in its own scope, which differs from `name` under `use m, new => old`. |
| `access` | `Public` or `Private`. |

### Return values

None.

## Description

Ordinary symbol lookup walks from a symbol table to its parents. That finds a
module variable used inside a procedure of the same module, so no
**ExternalSymbol** is needed there. It does not find a symbol that belongs to
another module, because that module is not a parent of the current scope.

**ExternalSymbol** closes that gap: `use m, only: f` puts an **ExternalSymbol**
named `f` into the using scope, pointing at the real symbol in `m`. Every
reference in that scope then names the local **ExternalSymbol**, so every
symbol a program unit refers to is reachable from its own symbol table.

## Examples

```clojure
(ExternalSymbol
  :parent_symtab 3
  :name "reset"
  :external (SymbolRef 1 "reset")
  :module_name "m"
  :scope_names []
  :original_name "reset"
  :access :Public
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/externalsymbol.asr
:language: clojure
```

## See Also

[Module](Module.md), [Function](Function.md), [Variable](Variable.md)
