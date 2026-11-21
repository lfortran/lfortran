# pdt_template_recipe

The inner PDT members whose kind arguments depend on a template's parameters.

## Declaration

### Syntax

```text
pdt_template_recipe = (identifier template_name, pdt_member_recipe* members)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `template_name` | A PDT template defined by the containing module. |
| `members` | Deferred instantiation details for its kind-dependent members. |

### Return values

None.

## Description

The original AST visitor's transient pending-member map is not available after
a module-file reload. This recipe preserves the member requests as structured
module metadata for the same ASR specialization algorithm.
