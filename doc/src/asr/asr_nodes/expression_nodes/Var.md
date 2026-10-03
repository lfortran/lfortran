# Var

A reference to a variable.

## Declaration

### Syntax

```text
Var(symbol v)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the [Variable](../symbol_nodes/Variable.md) symbol, or another symbol standing for one, such as an [ExternalSymbol](../symbol_nodes/ExternalSymbol.md). |

### Return values

The value of the expression.

## Description

**Var** holds nothing but the symbol. The type, the intent, the storage and
everything else are read from the
[Variable](../symbol_nodes/Variable.md) it points at, so there is exactly one
place where a variable's properties live and no way for a reference to
disagree with the declaration.

In ASR text the symbol is written `(SymbolRef id "name")`, naming the symbol
table and the key.

## Examples

```clojure
(Var
  :v (SymbolRef 1 "x")
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/program.asr
:language: clojure
```

## See Also

[Variable](../symbol_nodes/Variable.md), [ArrayItem](ArrayItem.md), [StructInstanceMember](StructInstanceMember.md)
