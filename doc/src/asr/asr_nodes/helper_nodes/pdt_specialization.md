# pdt_specialization

A concrete, foreign-owned PDT specialization required by a dependent module.

## Declaration

### Syntax

```text
pdt_specialization = (identifier owner_module, identifier template_name, expr* kinds)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `owner_module` | The module that defines the original PDT template, not the module importing it. |
| `template_name` | The original type name in that module, independent of local renames. |
| `kinds` | Constant integer kind arguments, including inherited parameters in parent-first order. |

### Return values

None.

## Description

The tuple identifies a canonical specialization without parsing a generated
symbol name. Its definition is materialized in `owner_module`'s in-memory
symbol table before the importing module's ExternalSymbols are resolved. It
does not rewrite the defining module's already-created module file. The
importing module lists `owner_module` as a dependency even when it reaches the
template through another imported module.
