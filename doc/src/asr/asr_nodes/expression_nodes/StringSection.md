# StringSection

A substring.

## Declaration

### Syntax

```text
StringSection(expr arg, expr? start, expr? end, expr? step, ttype type,
    expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the string. |
| `start` | the first index, counting from one. |
| `end` | the last index. |
| `step` | the stride. It must be present; one for an ordinary substring. |
| `type` | the type of the result, whose length must be given explicitly. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`s(i:j)`. The result is a value, not a reference: assigning to a substring is
an [Assignment](../statement_nodes/Assignment.md) whose target is the
**StringSection**.

The verifier requires both `step` and a length expression on the result type,
so that a backend never has to work out how long the result is.

## Examples

```clojure
(StringSection
  :arg (Var
    :v (SymbolRef 1 "s")
  )
  :start (IntegerConstant
    :n 2
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :end (IntegerConstant
    :n 4
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :step (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (String
    :kind 1
    :len (IntegerConstant
      :n 3
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :len_kind :ExpressionLength
    :physical_type :DescriptorString
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/string_expr.asr
:language: clojure
```

## See Also

[StringItem](StringItem.md), [StringLen](StringLen.md), [ArraySection](ArraySection.md)
