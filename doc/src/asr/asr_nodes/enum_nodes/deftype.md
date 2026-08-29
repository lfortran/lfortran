# deftype

Whether a procedure has a body here, and whether its signature is known.

## Declaration

### Syntax

```text
deftype = Implementation | Interface | ImplicitInterface
```

### Values

| Value | Meaning |
|----------|-------------|
| `Implementation` | the body is present in this ASR. |
| `Interface` | only the signature is present, and the signature is complete. |
| `ImplicitInterface` | neither a body nor a signature. The procedure was declared `external` with no interface, so only its result type is known; the dummy arguments are unknown. |

### Return values

None. An enumeration value is not evaluated.

## Description

An interface block, an external procedure with an explicit interface and a
procedure read from a module file as interface ASR are all `Interface`. The
distinction is not the same as the [abi](abi.md): a procedure may have a body
and still use a foreign ABI.

`ImplicitInterface` is not a third kind of declaration but the absence of one.
It records that this ASR does not know what the procedure takes. An empty
`arg_types` on a `FunctionType` otherwise means "takes no arguments", and that
is a different statement about the program; `ImplicitInterface` exists so the
two are never confused.

## ImplicitInterface

### When it may be created

Exactly one construct produces it: a procedure declared `external` with no
interface and no accessible definition, such as

```fortran
integer, external :: f
```

or a bare `external f` with the type supplied by an `implicit` rule. It is
created only when `--implicit-interface` is passed. Without that flag the same
declaration is a semantic error (*function interface must be specified
explicitly*), so an ASR produced under the default options never contains one.

The only place that creates it is `create_external_function` in the Fortran
frontend (`src/lfortran/semantics/ast_common_visitor.h`). No ASR pass and no
part of `libasr` may introduce one, and no pass may turn an `Interface` back
into an `ImplicitInterface`.

This is Fortran's own implicit interface (F2018 15.4.2.2, 15.4.3.5). The
declaration supplies the result type and nothing else: the number, types,
kinds, ranks, intents and attributes of the dummy arguments are all unknown.
F2018 15.5.2 states the rules a conforming *program* must satisfy at such a
reference, but a compiler that cannot see the definition cannot check them, so
the reference is not argument-checked against this symbol. It is not checked
against nothing, though — see the lowering rule below.

### Invariants

A `Function` with `deftype = ImplicitInterface`:

* has an empty `arg_types` and `n_args == 0`, which must be read as *unknown*,
  never as *none*;
* has `abi = BindC`, so that every reference to the name reaches one link-time
  symbol;
* has no body, and `n_body == 0`;
* is **never the target of a call**. `asr_verify.cpp` rejects a `FunctionCall`
  or `SubroutineCall` whose `name` resolves to one;
* is **never code-generated**. Every backend (LLVM, C/C++, MLIR, WASM) returns
  from `visit_Function` immediately, and `subroutine_from_function` skips it.
  It may still be given a link name in LLVM so that its address can be taken
  when it is passed as an actual argument, but a competing signature is never
  invented for it.

`ASRUtils::is_bare_implicit_interface` is the single predicate for this; do not
compare the deftype by hand.

### How lowering uses it: the interface is built at the reference

An `ImplicitInterface` symbol is a placeholder for a result type, not a
callable procedure. Every reference synthesizes its own complete `Interface`
`Function` from the actual arguments at that reference, and calls *that*. So

```fortran
r = f(1, 2)
```

produces a `Function f` with `deftype = Interface` and two `integer(4)` dummies
in the referencing scope, and the `FunctionCall` names it. The call therefore
agrees with its callee, exactly as any other call in ASR does, and the ordinary
argument checks apply to it. Both symbols carry the same `bindc_name`, so they
resolve to the same procedure at link time.

Within a single scope this means the placeholder does not survive: the first
reference overwrites it with the inferred `Interface`. A final ASR for

```fortran
program p
  implicit none
  integer, external :: f
  integer :: r
  r = f(1, 2)
end program p
```

contains one symbol `f`, with `deftype = Interface` and two arguments. There is
no `ImplicitInterface` left in it.

### Why the value is needed at all

The placeholder survives only where this translation unit has no reference to
infer from — chiefly a module that declares an external for the benefit of its
users:

```fortran
module m
  integer, external :: f
end module m
```

Nothing in `m` calls `f`, so nothing can supply a signature. `m.mod` must still
record `f`, because a program that uses the module needs its result type
(consider `character(len=80), external :: get_libvers`, where an implicit rule
would give the wrong type). What is recorded has to be distinguishable from a
genuine zero-argument interface, or

```fortran
program p
  use m
  integer :: r
  r = f(1, 2)
end program p
```

is rejected as *More actual than formal arguments in procedure call*, while
accepting it by treating every empty argument list as unknown would stop
checking real zero-argument interfaces. `ImplicitInterface` is the third state
that makes the module file able to say which one it holds, and the caller then
applies the lowering rule above.

Because it must survive a module-file round trip,
`SymbolTable::mark_all_variables_external` leaves the deftype alone rather than
rewriting it to `Interface` as it does for other procedures.

### Once the signature becomes known

A reference is not the only thing that can supply a signature. When the
procedure is passed as an actual argument to a procedure that does call it, the
signature propagates back from the dummy. At that point the arguments are
filled in and the deftype becomes `Interface`; the symbol stops being a
placeholder and the invariants above no longer apply to it.

### Limitation

Two references in the same scope that pass different actual types cannot both
be satisfied: the first fixes the inferred `Interface`, and the second is
reported as a type mismatch. Such a program is not standard-conforming (F2018
15.5.2.5 requires the actual arguments to agree with the definition's dummies,
so the two references cannot both agree), and gfortran needs
`-fallow-argument-mismatch` to accept it. Supporting that shape is
[lfortran/lfortran#12621](https://github.com/lfortran/lfortran/pull/12621).

## See Also

[FunctionType](../type_nodes/FunctionType.md), [Function](../symbol_nodes/Function.md), [abi](abi.md)
