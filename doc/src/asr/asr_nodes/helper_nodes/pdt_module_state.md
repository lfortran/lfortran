# pdt_module_state

The specialization dependencies and deferred template-member arguments
persisted with a [Module](../symbol_nodes/Module.md).

## Declaration

### Syntax

```text
pdt_module_state = (pdt_specialization* specializations, pdt_template_recipe* templates)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `specializations` | Concrete specializations this module uses but a different module owns. |
| `templates` | Deferred arguments of inner PDT members of templates this module defines. |

### Return values

None.

## Description

On reload, the compiler first loads module dependencies and creates any
requested specializations in their original defining scopes. Only then can it
resolve this module's external type references. Requests for the same template
and kind tuple reuse a single definition in the owner's loaded symbol table.
