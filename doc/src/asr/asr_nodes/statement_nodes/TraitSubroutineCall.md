# TraitSubroutineCall

Invoke an ordinary subroutine selected dynamically from a view's witness.

## Declaration

### Syntax

```text
TraitSubroutineCall(symbol name, int slot, call_arg* args)
```

The first argument is a borrowed trait view. `name` must be the normalized
interface at `slot` in its canonical runtime contract. The remaining arguments
have the message's normal types, order, and association attributes. Scalars are
integer, real, complex, logical, character, or nonpolymorphic derived-type
values that retain their ordinary Fortran intents and calling conventions.
Arrays are read-only (`intent(in)`), required, nonallocatable, nonpointer
assumed-shape arrays of an intrinsic numeric or logical type, of any rank and
optionally with a declared lower bound such as `x(0:)`; they are passed by
reference as descriptors. As in an ordinary call, an actual array of another
physical representation, such as a fixed-size array, reaches its dummy through
an explicit `ArrayPhysicalCast`.

Unlike [TraitFunctionCall](../expression_nodes/TraitFunctionCall.md), the call
has no result, and `slot` is always an ordinary slot without type arguments:
member slots of closed generic messages, erased generic slots, and
`TraitDeferredCall` exist only for functions. Generic subroutine messages,
BIND(C) messages, and messages with any other dummy, such as an optional,
polymorphic, allocatable, pointer, explicit-shape, assumed-size, or
assumed-rank one or an array that is not `intent(in)`, are outside the runtime
slot surface: a trait with such a message has no runtime contract, and using it
as a runtime view is diagnosed as not implemented.

Verification checks the slot against the actual view's canonical contract and
then applies ordinary call checks; an ordinary `SubroutineCall` cannot name a
contract interface. The call loads the passed witness even for a NOPASS
implementation; NOPASS only removes the receiver from the adapter's final
concrete call. Normal and binary ASR serialization preserve this unresolved
dynamic operation.

See [TraitFunctionCall](../expression_nodes/TraitFunctionCall.md).
