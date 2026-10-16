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
Arrays are required, nonallocatable, nonpointer assumed-shape arrays of an
intrinsic numeric or logical type or of character of a constant length, of any
rank and optionally with a declared lower bound such as `x(0:)`, that also
retain their ordinary intents; they are passed by reference as descriptors, so
an `intent(inout)`, `intent(out)` or omitted-intent array defines the elements
of the caller's actual in place. As in an ordinary call, an actual array of
another physical representation, such as a fixed-size array, reaches its dummy
through an explicit `ArrayPhysicalCast`, and the actual of an `intent(inout)`
or `intent(out)` array must be definable. A message that is not generic may
also take such arrays of explicit shape, whose bounds can refer to its other
dummies; they are passed by data, like those of an ordinary call, and the
`array_passed_in_function_call` pass copies a noncontiguous actual into a
contiguous temporary before the call and, unless the dummy is `intent(in)`,
back after it. A message that is not generic may also take bare `intent(in)` or
`intent(inout)` views of runtime traits; each such actual is a view of the
dummy's exact contract, an explicit `TraitBorrow`, `TraitPack` or
`TraitProject` or a forwarded view, as for a bare view dummy of an ordinary
call, and is passed by reference.

Unlike [TraitFunctionCall](../expression_nodes/TraitFunctionCall.md), the call
has no result, and `slot` is always an ordinary slot without type arguments:
member slots of closed generic messages, erased generic slots, and
`TraitDeferredCall` exist only for functions. Generic subroutine messages,
BIND(C) messages, and messages with any other dummy, such as an optional,
allocatable, pointer, assumed-size, or assumed-rank one, an explicit-shape
array of a generic message, a polymorphic dummy of a derived type, or an
allocatable, pointer, TARGET or VOLATILE view, are outside the runtime slot
surface: a trait with such a message has no runtime contract, and using it as
a runtime view is diagnosed as not implemented.

Verification checks the slot against the actual view's canonical contract and
then applies ordinary call checks; an ordinary `SubroutineCall` cannot name a
contract interface. The call loads the passed witness even for a NOPASS
implementation; NOPASS only removes the receiver from the adapter's final
concrete call. Normal and binary ASR serialization preserve this unresolved
dynamic operation.

See [TraitFunctionCall](../expression_nodes/TraitFunctionCall.md).
