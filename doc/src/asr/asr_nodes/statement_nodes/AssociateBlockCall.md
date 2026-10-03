# AssociateBlockCall

Runs the body of an `associate` construct.

## Declaration

### Syntax

```text
AssociateBlockCall(symbol m)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `m` | the [AssociateBlock](../symbol_nodes/AssociateBlock.md) symbol holding the scope and the statements. |

### Return values

None.

## Description

Like [BlockCall](BlockCall.md), this marks where the construct runs while the
scope and the statements live in the symbol.

## Examples

```clojure
(AssociateBlockCall
  :m (SymbolRef 1 "block")
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/associateblock.asr
:language: clojure
```

## See Also

[AssociateBlock](../symbol_nodes/AssociateBlock.md), [BlockCall](BlockCall.md), [Associate](Associate.md)
