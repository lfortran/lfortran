# StringCompare

A comparison of two strings.

## Declaration

### Syntax

```text
StringCompare(expr left, cmpop op, expr right, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `left` | the left operand. |
| `op` | the comparison; see [cmpop](../enum_nodes/cmpop.md). |
| `right` | the right operand. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

Strings are compared character by character, and the shorter one is treated as
padded with blanks, so `"a" < "ab"` is true. The result is logical.

## Examples

```clojure
(StringCompare
  :left (Var
    :v (SymbolRef 1 "s")
  )
  :op :Eq
  :right (StringConstant
    :s "world"
    :type (String
      :kind 1
      :len (IntegerConstant
        :n 5
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :len_kind :ExpressionLength
      :physical_type :DescriptorString
    )
  )
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/string_expr.asr
:language: clojure
```

## See Also

[cmpop](../enum_nodes/cmpop.md), [StringConcat](StringConcat.md), [StringContains](StringContains.md)
