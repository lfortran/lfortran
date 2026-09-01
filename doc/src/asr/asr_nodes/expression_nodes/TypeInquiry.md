# TypeInquiry

A question about a type rather than about a value.

## Declaration

### Syntax

```text
TypeInquiry(int inquiry_id, ttype arg_type, expr? arg, ttype type,
    expr value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `inquiry_id` | which inquiry this is, as the integer id of the inquiry registry. |
| `arg_type` | the type being asked about. |
| `arg` | the expression the type was taken from, when there was one; it is not evaluated. |
| `type` | the type of the result. |
| `value` | the answer. It is always known at compile time, so unlike most expressions this member is required. |

### Return values

The value of the expression.

## Description

`kind(x)`, `huge(x)`, `epsilon(x)` and the rest ask about the type of their
argument, not about its value. The argument is kept in `arg` for diagnostics
and unparsing, but it is not evaluated, and the answer is already in `value`.

## Examples

```clojure
(TypeInquiry
  :inquiry_id 1
  :arg_type (Real
    :kind 4
  )
  :arg (Var
    :v (SymbolRef 1 "x")
  )
  :type (Integer
    :kind 4
  )
  :value (IntegerConstant
    :n 4
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/intrinsic_expr.asr
:language: clojure
```

## See Also

[IntrinsicElementalFunction](IntrinsicElementalFunction.md), [SizeOfType](SizeOfType.md), kinds
