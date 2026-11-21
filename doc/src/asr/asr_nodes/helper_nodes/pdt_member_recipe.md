# pdt_member_recipe

A kind-dependent inner PDT component of a defining module's PDT template.

## Declaration

### Syntax

```text
pdt_member_recipe = (identifier member_name, identifier type_name, expr* kinds)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `member_name` | The declared component of the containing template. |
| `type_name` | The inner PDT's name in the template's scope; its import may be private or renamed. |
| `kinds` | Constant integer arguments, including placeholders for the enclosing template's kind parameters. |

### Return values

None.

## Description

Substituting the concrete outer kind values yields the inner specialization
request. Keeping the original scope's name lets a later importer resolve the
same type without reconstructing the source AST or guessing from mangled names.
