# FunctionPointerCast

Views a procedure through a different signature.

## Declaration

### Syntax

```text
FunctionPointerCast(expr arg, symbol to, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the procedure being viewed. |
| `to` | the [Function](../symbol_nodes/Function.md) whose signature this view has. It is an `Interface`; its body, if any, is not what is called through the cast. |
| `type` | the type of the result: the [FunctionType](../type_nodes/FunctionType.md) of `to`. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The procedure, typed as `to`.

## Description

Nothing is converted. The procedure's address is unchanged; only the signature
the call site uses to reach it differs. It is the procedure-valued counterpart
of [BitCast](BitCast.md), and the same warning applies: whether the call is
correct is not decided here.

It exists for one situation. Under `--implicit-interface`, a procedure declared
`external` with no interface (see
[deftype](../enum_nodes/deftype.md)) gets its signature from the actual
arguments at each reference. When two references in the same scope disagree,
one inferred `Interface` cannot serve both. The first-inferred signature stays
the canonical procedure under the user-visible name; each later reference that
disagrees gets its own `Interface` symbol, and reaches the procedure through a
`FunctionPointerCast` to it. The call is then a call of a procedure pointer
associated with the cast, so the call agrees with its callee, which is the
invariant every call in ASR has to satisfy.

Such a program is not standard-conforming: F2018 15.5.2.5 requires the actual
arguments at every reference to agree with the dummies of the definition, so
two references that disagree with each other cannot both agree with it.
gfortran accepts it with `-fallow-argument-mismatch`. This node is the model
LFortran uses for it, and is the model separate compilation gives anyway when
the definition is not in this translation unit.

Do not produce this node for anything else. A conversion between data types is
[Cast](Cast.md); reinterpreting the bits of a value is [BitCast](BitCast.md);
a procedure with a known interface needs no cast at all.

## See Also

[BitCast](BitCast.md), [FunctionCall](FunctionCall.md), [FunctionType](../type_nodes/FunctionType.md), [deftype](../enum_nodes/deftype.md), [Function](../symbol_nodes/Function.md)
