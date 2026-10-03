# BlockCall

Runs the body of a `block` construct.

## Declaration

### Syntax

```text
BlockCall(int label, symbol m)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label of the `block` statement, or `-1`. |
| `m` | the [Block](../symbol_nodes/Block.md) symbol holding the scope and the statements. |

### Return values

None.

## Description

The statements of a `block` live in the [Block](../symbol_nodes/Block.md)
symbol, because the construct has a scope of its own. **BlockCall** is what
marks the position in the enclosing statement list where they run.

## Examples

```clojure
(BlockCall
  :label -1
  :m (SymbolRef 1 "block")
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/block.asr
:language: clojure
```

## See Also

[Block](../symbol_nodes/Block.md), [AssociateBlockCall](AssociateBlockCall.md)
