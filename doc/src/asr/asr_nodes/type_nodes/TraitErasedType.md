# TraitErasedType

A value of an erased protocol binder, described by a dictionary.

## Declaration

### Syntax

```text
TraitErasedType(identifier parameter)
```

`parameter` names the erased binder, or `itself` in the slots of a
[TraitDictionaryContract](../symbol_nodes/TraitDictionaryContract.md). A
procedure with an erased type has one trailing dictionary dummy per erased
binder, in binder order, of type `TraitDictionaryType(contract, parameter)`;
it describes the size, initializer and operations of the values.

Erased storage is restricted to read-only scalar and assumed-shape dummies,
the receiver of a dictionary slot and its adapter, which is `intent(inout)`
for an `intent(inout)` or `intent(out)` message receiver, scalar locals and a
scalar result. At a call, an erased dummy takes the type
that the call's dictionary actual for the same parameter describes: the
implementation of a `TraitDictionaryValue`, the caller's erased type for a
forwarded dictionary dummy, or the binder of a deferred dictionary. Types of
different parameters are different even if their dictionaries agree.

Only a dictionary knows how to copy or interpret erased values, so the
verifier also requires that an array of erased values is a
`DescriptorArray` held by a dummy or a pointer association (a pass may alias
a section, never copy it), that no array constructor, function result or
allocatable has an erased element type, that erased values are not operands
of intrinsic functions, string formatting or input/output, and that an
erased actual argument meets an erased dummy.

In LLVM a scalar is passed by address and an array by a descriptor whose
element type is a byte and whose offset and strides are in bytes, so sections,
reversed strides and empty arrays need no element size. An erased result is
written to storage the caller provides, sized by the dictionary when the
caller's type is erased too. Assignment copies the dictionary's size.
