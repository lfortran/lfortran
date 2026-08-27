# Block

The scope of a `block` construct.

## Declaration

### Syntax

```text
Block(symbol_table symtab, identifier name, stmt* body)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the construct, holding the variables it declares. |
| `name` | a generated name for the block, unique in its scope. |
| `body` | the statements of the construct. |

### Return values

None.

## Description

A `block` construct declares variables of its own, so like
[AssociateBlock](AssociateBlock.md) it is a symbol holding a scope rather than
a statement holding a list. [BlockCall](../statement_nodes/BlockCall.md) marks
where in the enclosing statement list the block runs.

The variables of the block are ordinary [Variable](Variable.md) symbols in
`symtab`. They are allocated when the block is entered and are not visible
outside it.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/block.asr
:language: clojure
```

## See Also

[BlockCall](../statement_nodes/BlockCall.md), [AssociateBlock](AssociateBlock.md), [Variable](Variable.md)
