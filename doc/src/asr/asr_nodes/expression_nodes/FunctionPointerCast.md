# FunctionPointerCast

Views a procedure through a different procedure type.

## Declaration

### Syntax

```text
FunctionPointerCast(expr arg, symbol? to, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg` | the procedure being viewed: a procedure, a dummy procedure or a procedure pointer. |
| `to` | the [Function](../symbol_nodes/Function.md) whose signature this view has. It is an explicit interface; its body, if any, is not what is called through the cast. `nil` for a cast to the opaque procedure type. |
| `type` | the type of the result: the [FunctionType](../type_nodes/FunctionType.md) of `to`, or, without `to`, the opaque procedure type (deftype `ImplicitInterface`, see [deftype](../enum_nodes/deftype.md)). |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The procedure, typed as `type`.

## Description

Nothing is converted. The procedure's address is unchanged; only the signature
used to reach it differs. It is the procedure-valued counterpart of
[BitCast](BitCast.md), and the same warning applies: whether the call is
correct is not decided here.

It exists for procedures with an implicit interface (see
[deftype](../enum_nodes/deftype.md)), whose type says nothing about their
arguments:

* **Calls.** Each reference to a procedure with an implicit interface builds an
  `Interface` from its own actual arguments (and result type), filed in the
  calling procedure as `name@fpcast` and shared by references with the same
  signature. The procedure is associated with a procedure-pointer temporary
  through a `FunctionPointerCast` to that interface right before the statement
  (before each evaluation of a DO WHILE condition), and the call is a call of
  that temporary, so the call agrees with its callee. The temporary belongs to
  the scope of the statements it is associated in (e.g. a statement function
  has its own). A procedure with an implicit interface is not a specification
  function, so it is never referenced in a specification expression.
* **Procedure actuals and pointer targets.** A procedure passed to a dummy
  procedure, or associated with a procedure pointer, whose type differs from
  its own is cast to the type of the dummy or pointer: to the opaque type,
  without `to`, for a dummy with an implicit interface, and to the dummy's
  interface (or a copy of it in the caller) for a procedure with an implicit
  interface passed to a dummy with an explicit one.

A cast to a type other than the opaque one needs `to`: the interface carries
the argument declarations (derived types, character and array physical types)
the lowering of the signature depends on.

Two references with different argument lists are not standard-conforming
(F2018 15.5.2.5 requires the actual arguments at every reference to agree with
the dummies of the definition). gfortran accepts such a program with
`-fallow-argument-mismatch`; this node is the model LFortran uses for it, and
the model separate compilation gives anyway.

Do not produce this node for anything else. A conversion between data types is
[Cast](Cast.md); reinterpreting the bits of a value is [BitCast](BitCast.md);
a procedure passed where its own explicit interface is expected needs no cast.

## See Also

[BitCast](BitCast.md), [FunctionCall](FunctionCall.md), [FunctionType](../type_nodes/FunctionType.md), [deftype](../enum_nodes/deftype.md), [Function](../symbol_nodes/Function.md)
