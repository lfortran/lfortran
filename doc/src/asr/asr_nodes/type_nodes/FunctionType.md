# FunctionType

The signature of a procedure.

## Declaration

### Syntax

```text
FunctionType(ttype* arg_types, ttype? return_var_type, abi abi,
    deftype deftype, string? bindc_name, bool elemental, bool pure,
    bool module, bool inline, bool static, symbol* restrictions,
    bool is_restriction, bool external_abi)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `arg_types` | the types of the dummy arguments, in order. |
| `return_var_type` | the type of the result, or `nil` for a subroutine. |
| `abi` | the ABI of the procedure; see the [ABI section](../../asr.md) of the ASR overview. |
| `deftype` | `Implementation` when the body is present, `Interface` when only the signature is, `ImplicitInterface` when neither is known; see [deftype](../enum_nodes/deftype.md). |
| `bindc_name` | the linker name given by `bind(c, name=...)`. |
| `elemental` | `true` for an `elemental` procedure, which applies to each element of an array argument. |
| `pure` | `true` for a `pure` procedure. |
| `module` | `true` for a module procedure whose interface is declared in a module and whose body is in a submodule. |
| `inline` | `true` when the procedure is marked for inlining. |
| `static` | `true` when the procedure's local variables are allocated statically rather than on the stack. |
| `restrictions` | for a procedure of a generic [Template](../symbol_nodes/Template.md), the operations its type parameters must provide. |
| `is_restriction` | `true` when this signature is itself one of those required operations rather than a procedure with an implementation. |
| `external_abi` | `true` for a separately compiled external procedure: a subprogram defined at the top level, an interface body in a plain (non-abstract, non-module) interface block, or an interface synthesized for a procedure referenced without an explicit one. Such procedures use the classic Fortran external ABI, in which a CHARACTER dummy is passed as a data pointer with its length as a hidden trailing argument, so that they interoperate with gfortran and flang. Compiler-synthesized procedures leave this `false`. |

### Return values

None. A type is not evaluated.

## Description

Everything about a procedure other than its body and its symbols lives here,
so a call site can be checked against the signature without looking at the
procedure's symbol table.

A subroutine is a signature with no `return_var_type`. `deftype` says whether
there is a body: an interface, and a procedure loaded from a module file as
interface ASR, are `Interface`. An empty `arg_types` means the procedure takes
no arguments, except under `deftype = ImplicitInterface`, where it means the
arguments are not known here.

## Examples

```clojure
(FunctionType
  :arg_types [
    (Integer
      :kind 4
    )
    (Integer
      :kind 4
    )
  ]
  :return_var_type (Integer
    :kind 4
  )
  :abi :Source
  :deftype :Implementation
  :bindc_name nil
  :elemental false
  :pure false
  :module false
  :inline false
  :static false
  :restrictions []
  :is_restriction false
  :external_abi false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/function.asr
:language: clojure
```

## See Also

[Function](../symbol_nodes/Function.md), [FunctionCall](../expression_nodes/FunctionCall.md), [deftype](../enum_nodes/deftype.md), [abi](../enum_nodes/abi.md), [FunctionParam](../expression_nodes/FunctionParam.md)
