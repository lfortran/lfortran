# do_loop_head

The control of a counted loop.

## Declaration

### Syntax

```text
do_loop_head = (expr? v, expr? start, expr? end, expr? increment)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `v` | the loop variable. |
| `start` | its first value. |
| `end` | its last value. |
| `increment` | the step, or `nil` for one. |

### Return values

None.

## Description

Shared by [DoLoop](../statement_nodes/DoLoop.md),
[DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md) and
[ForAllSingle](../statement_nodes/ForAllSingle.md), so that one form of loop
control serves all three.

The trip count is computed from `start`, `end` and `increment` before the
first iteration. A head whose members are all `nil` is an infinite loop.

## Examples

```clojure
(do_loop_head
  :v (Var
    :v (SymbolRef 1 "i")
  )
  :start (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :end (IntegerConstant
    :n 10
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :increment (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/control_stmt.asr
:language: clojure
```

## See Also

[DoLoop](../statement_nodes/DoLoop.md), [DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md), [ForAllSingle](../statement_nodes/ForAllSingle.md)
