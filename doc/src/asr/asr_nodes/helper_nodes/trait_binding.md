# trait_binding

A concrete witness for a trait message.

## Declaration

### Syntax

```text
trait_binding = (symbol member, symbol procedure,
    identifier? self_argument, bool is_nopass)
```

### Arguments

| Argument | Description |
| --- | --- |
| `member` | Canonical reference to the trait's abstract message signature. |
| `procedure` | Concrete implementing procedure. |
| `self_argument` | Name of its passed-object dummy, absent for `nopass`. |
| `is_nopass` | Whether the implementation takes no receiver. |

### Return values

None.

## Description

The receiver can occupy a non-first argument position. Static specialization
uses this metadata to construct an ordinary typed procedure adapter instead
of making the backend infer receiver positions.

## Examples

See the [static traits guide](../../traits.md).

## See Also

[TraitImplementation](../symbol_nodes/TraitImplementation.md)
