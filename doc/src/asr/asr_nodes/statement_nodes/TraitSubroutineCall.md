# TraitSubroutineCall

Invoke an ordinary subroutine selected dynamically from a view's witness.

## Declaration

### Syntax

```text
TraitSubroutineCall(symbol name, int slot, call_arg* args)
```

The first argument is a borrowed trait view. `name` must be the normalized
interface at `slot` in its canonical runtime contract. The remaining scalar
arguments retain ordinary Fortran intents and calling conventions.

The call loads the passed witness even for a NOPASS implementation; NOPASS only
removes the receiver from the adapter's final concrete call. Normal and binary
ASR serialization preserve this unresolved dynamic operation.

See [TraitFunctionCall](../expression_nodes/TraitFunctionCall.md).
