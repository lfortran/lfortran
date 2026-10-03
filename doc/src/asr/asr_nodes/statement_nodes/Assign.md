# Assign

The obsolescent `assign` statement: stores a label in an integer variable.

## Declaration

### Syntax

```text
Assign(int label, identifier variable)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `label` | the statement label being stored. |
| `variable` | the name of the integer variable it is stored in. |

### Return values

None.

## Description

`assign 100 to label` stores a statement label rather than an integer value in
`label`, for use by an assigned `go to`. The feature was deleted in Fortran
95 and is supported for legacy code.

The variable holds a label, not a number, so the only thing that may be done
with it is an assigned `go to`; using it in arithmetic is not meaningful.

## Examples

```clojure
(Assign
  :label 100
  :variable "label"
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/goto_stmt.asr
:language: clojure
```

## See Also

[GoTo](GoTo.md), [GoToTarget](GoToTarget.md), [Variable](../symbol_nodes/Variable.md)
