# GoToTarget

A labelled statement: the target of zero or more jumps.

## Declaration

### Syntax

```text
GoToTarget(int id, identifier name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `id` | the identifier [GoTo](GoTo.md) uses to reach this statement. It is unique within the procedure. |
| `name` | the label as it was written. |

### Return values

None.

## Description

**GoToTarget** does nothing when it is executed. It marks a position in the
statement list so that a [GoTo](GoTo.md), an
[IfArithmetic](IfArithmetic.md) or an `err=` specifier can name it.

A target may be jumped to from several places, or from none, in which case it
is dead and later passes may remove it.

## Examples

```clojure
(GoToTarget
  :id 100
  :name "100"
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/goto_stmt.asr
:language: clojure
```

## See Also

[GoTo](GoTo.md), [IfArithmetic](IfArithmetic.md)
