# IntrinsicElementalFunction

A call to an elemental intrinsic function.

## Declaration

### Syntax

```text
IntrinsicElementalFunction(int intrinsic_id, expr* args,
    int overload_id, ttype? type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `intrinsic_id` | which intrinsic this is, as the integer id of the intrinsic registry. |
| `args` | the actual arguments. |
| `overload_id` | which signature was selected, when the intrinsic has several. |
| `type` | the type of the result; `nil` before the frontend has resolved it. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

An intrinsic function is not a symbol: there is no
[Function](../symbol_nodes/Function.md) to point at, so the call names the
intrinsic by id. The registry in
`src/libasr/pass/intrinsic_function_registry.h` owns the mapping from id to
name, to the compile time evaluation rule and to the lowering.

Elemental means the intrinsic applies to each element of an array argument.
The node is used for the scalar case as well;
[IntrinsicArrayFunction](IntrinsicArrayFunction.md) is for intrinsics such as
`sum` that reduce an array instead.

## Examples

```clojure
(IntrinsicElementalFunction
  :intrinsic_id 3
  :args [
    (Var
      :v (SymbolRef 1 "x")
    )
  ]
  :overload_id 0
  :type (Real
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

[IntrinsicArrayFunction](IntrinsicArrayFunction.md), [IntrinsicImpureFunction](IntrinsicImpureFunction.md), [FunctionCall](FunctionCall.md), [TypeInquiry](TypeInquiry.md)
