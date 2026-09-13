# IntrinsicImpureSubroutine

A call to an intrinsic subroutine.

## Declaration

### Syntax

```text
IntrinsicImpureSubroutine(int sub_intrinsic_id, expr* args,
    int overload_id)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `sub_intrinsic_id` | which intrinsic subroutine this is, as the integer id of the intrinsic registry. |
| `args` | the actual arguments. |
| `overload_id` | which signature of the intrinsic was selected, when it has several. |

### Return values

None.

## Description

Intrinsic subroutines such as `random_number` and `date_and_time` are not
symbols: there is no [Function](../symbol_nodes/Function.md) to point at, so
the call names the intrinsic by id instead.

A later pass either lowers the call into ASR of its own or leaves it for the
backend, which is why the id, and not a name, is what is stored: the registry
in `src/libasr/pass/intrinsic_function_registry.h` owns the mapping.

## Examples

```clojure
(IntrinsicImpureSubroutine
  :sub_intrinsic_id 0
  :args [
    (Var
      :v (SymbolRef 1 "x")
    )
  ]
  :overload_id 0
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/intrinsicimpuresubroutine_stmt.asr
:language: clojure
```

## See Also

[IntrinsicElementalFunction](../expression_nodes/IntrinsicElementalFunction.md), [IntrinsicImpureFunction](../expression_nodes/IntrinsicImpureFunction.md), [SubroutineCall](SubroutineCall.md)
