# StringContains

Whether one string occurs inside another.

## Declaration

### Syntax

```text
StringContains(expr substr, expr str, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `substr` | the string looked for. |
| `str` | the string looked in. |
| `type` | the logical type of the result. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The result is logical. Fortran's `index` returns a position instead and is an
intrinsic call; **StringContains** is the membership test LPython's `in`
needs.

## Examples

```clojure
(StringContains
  :substr (StringConstant
    :s "ell"
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
  )
  :str (Var
    :v (SymbolRef 1 "s")
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

[StringCompare](StringCompare.md), [StringItem](StringItem.md), [IntrinsicElementalFunction](IntrinsicElementalFunction.md)
