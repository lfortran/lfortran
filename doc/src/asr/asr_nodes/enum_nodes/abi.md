# abi

Where a symbol's implementation lives, and which calling convention reaches it.

## Declaration

### Syntax

```text
abi
    = Source
    | LFortranModule
    | GFortranModule
    | BindC
    | BindPython
    | BindJS
    | ExternalUndefined
    | Intrinsic
```

### Values

| Value | Meaning |
|----------|-------------|
| `Source` | the implementation is part of this ASR. A backend may use any convention it likes for it, and may inline or eliminate it. |
| `LFortranModule` | external, in an object file compiled by LFortran, reached with LFortran's own convention. |
| `GFortranModule` | external, in an object file compiled by GFortran, reached with GFortran's convention. |
| `BindC` | the C convention. This is what `bind(c)` and an `iso_c_binding` interface produce; a variable may be defined locally or declared with external storage. |
| `BindPython` | the implementation is Python source in the user's file, executed through CPython. LPython converts the arguments and the result. |
| `BindJS` | the implementation is provided by JavaScript, for the WebAssembly backend. |
| `ExternalUndefined` | external with no convention recorded yet. |
| `Intrinsic` | the language itself provides the implementation. There is no body, and a backend may implement it however it likes. |

### Return values

None. An enumeration value is not evaluated.

## Description

`Source` normally means the implementation is part of this ASR, and is what a
procedure compiled from the source at hand has. Module ABIs and
`ExternalUndefined` identify implementations defined elsewhere.

For variables, data ABI and declaration ownership are separate properties.
A `BindC` global may be defined locally with C-compatible layout and linkage.
An imported declaration keeps that ABI and sets
[Variable](../symbol_nodes/Variable.md)'s `external_storage` flag, independently
of its storage lifetime (including threadprivate storage).

When a module is compiled, its full ASR (`abi=Source`, bodies present) is
transformed into interface ASR (`abi=LFortranModule`, bodies empty), and both
are written to the module file. A program that uses the module gets the
interface form, which is why a symbol reached through
[ExternalSymbol](../symbol_nodes/ExternalSymbol.md) normally has a module ABI.

## See Also

[Function](../symbol_nodes/Function.md), [Variable](../symbol_nodes/Variable.md), [Module](../symbol_nodes/Module.md), [FunctionType](../type_nodes/FunctionType.md), [ExternalSymbol](../symbol_nodes/ExternalSymbol.md)
