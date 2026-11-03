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
| `increment` | the step. |

### Return values

None.

## Description

Shared by [DoLoop](../statement_nodes/DoLoop.md),
[DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md) and
[ForAllSingle](../statement_nodes/ForAllSingle.md), so that one form of loop
control serves all three.

The trip count is computed from `start`, `end` and `increment` before the
first iteration. A head whose members are all `nil` is an infinite loop.

### Normalized form

Every head is in a normalized form, so that passes and backends lower it as
given, without supplying a default step or converting types themselves. The
verifier checks that each head of a [DoLoop](../statement_nodes/DoLoop.md),
[DoConcurrentLoop](../statement_nodes/DoConcurrentLoop.md) and
[ForAllSingle](../statement_nodes/ForAllSingle.md) is either

* a counted head: `v` is a scalar variable, and `start`, `end` and
  `increment` are all present and scalar, each of exactly the type and kind
  of `v`. A step of one is written out as a constant `1` of that type; where
  the source mixes kinds, such as `do i = 1_8, n` with an `integer(4) :: i`,
  the expression is wrapped in an explicit [Cast](../expression_nodes/Cast.md)
  to the type of `v`; or
* the head of an infinite loop, `do ... end do`, with all four members `nil`.
  Only a **DoLoop** may have it.

`v` is usually an integer, but has to agree only with the other three, so the
deleted feature of a `real` DO variable, which the frontend still accepts,
gives a head of that real type.

Code that creates a loop builds the head with
`ASRUtils::make_DoLoop_t_util`, `ASRUtils::make_DoConcurrentLoop_t_util`,
`ASRUtils::make_ForAllSingle_t_util` or `ASRBuilder::DoLoop`, which normalize
it (`ASRUtils::normalize_do_loop_head`), rather than with the generated
`ASR::make_*_t` constructors. ASR written as text has to spell out the
normalized form, including the `increment`.

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
