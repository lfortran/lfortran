# IntrinsicImpureFunction

A call to an intrinsic function whose result depends on more than its arguments.

## Declaration

### Syntax

```text
IntrinsicImpureFunction(int impure_intrinsic_id, expr* args,
    int overload_id, ttype? type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `impure_intrinsic_id` | which intrinsic this is, as the integer id of the impure intrinsic registry. |
| `args` | the actual arguments. |
| `overload_id` | which signature was selected. |
| `type` | the type of the result; `nil` before it is resolved. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

`allocated`, `associated` and `is_iostat_end` read state rather than compute a
function of their arguments, so they can never be folded at compile time and
must not be moved out of a loop or duplicated. Keeping them in a separate node
means an optimisation pass cannot treat them as pure by accident.

## Examples

```clojure
(IntrinsicImpureFunction
  :impure_intrinsic_id 2
  :args [
    (Var
      :v (SymbolRef 1 "c")
    )
  ]
  :overload_id 0
  :type (Logical
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/intrinsic_expr.asr
:language: clojure
```

## See Also

[IntrinsicElementalFunction](IntrinsicElementalFunction.md), [IntrinsicImpureSubroutine](../statement_nodes/IntrinsicImpureSubroutine.md), [PointerAssociated](PointerAssociated.md)
