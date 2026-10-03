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
| `BindC` | external, with the C convention. This is what `bind(c)` and an `iso_c_binding` interface produce. |
| `BindPython` | the implementation is Python source in the user's file, executed through CPython. LPython converts the arguments and the result. |
| `BindJS` | the implementation is provided by JavaScript, for the WebAssembly backend. |
| `ExternalUndefined` | external with no convention recorded yet. |
| `Intrinsic` | the language itself provides the implementation. There is no body, and a backend may implement it however it likes. |

### Return values

None. An enumeration value is not evaluated.

## Description

Every value other than `Source` means the symbol is external: its
implementation is not in this ASR, and a variable with that ABI is not
allocated by this ASR either. `Source` means the opposite, and is what a
procedure compiled from the source at hand has.

When a module is compiled, its full ASR (`abi=Source`, bodies present) is
transformed into interface ASR (`abi=LFortranModule`, bodies empty), and both
are written to the module file. A program that uses the module gets the
interface form, which is why a symbol reached through
[ExternalSymbol](../symbol_nodes/ExternalSymbol.md) normally has a module ABI.

## See Also

[Function](../symbol_nodes/Function.md), [Variable](../symbol_nodes/Variable.md), [Module](../symbol_nodes/Module.md), [FunctionType](../type_nodes/FunctionType.md), [ExternalSymbol](../symbol_nodes/ExternalSymbol.md)
