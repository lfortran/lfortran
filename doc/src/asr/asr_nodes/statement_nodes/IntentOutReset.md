# IntentOutReset

Reset an existing polymorphic scalar on entry to an `intent(out)` procedure.

## Declaration

### Syntax

```text
IntentOutReset(expr object)
```

## Description

`object` is a nonpointer, nonallocatable scalar polymorphic variable or array
element. The `intent_out_deallocate` pass constructs this statement and
surrounds array elements with ordinary ASR loops over the dummy's bounds.
For an optional dummy, one presence check encloses the entry statements.

The dynamic type selects its defining `Struct.intent_out_reset` helper.
That subroutine takes one required scalar `type(T), intent(inout)` argument,
so invoking it does not recursively reset the object. Its ordinary ASR body
finalizes nonallocatable components, conditionally deallocates allocatable
components, and restores component defaults. Deallocation invokes the
allocated component's FINAL procedures. Pointer defaults change association,
not target ownership; fixed character storage remains owned by the object.
Null pointer defaults use the component expression's type in the helper's
scope, including imported procedure-interface bounds, rather than reusing
the default initializer's declaration-scope type.
Non-null procedure and data targets are copied into the receiving scope using
canonical symbol imports, then restored with ordinary pointer association.
This preserves saved target storage rather than assigning its initial value.

This is not destruction of the object or allocation of a replacement.
Root finalization is separate; this node does not implement dynamic root
FINAL dispatch. An intrinsic dynamic type of `class(*)` needs no reset.

LLVM lowers the node to an indirect call through the reset slot of the
dynamic type's vtable. A null slot means the dynamic type needs no component
cleanup or initialization; extensions are checked independently of their base.
Other backends must diagnose unsupported dispatch,
not drop the statement. Source Fortran should be emitted before this pass:
it retains `intent(out)` and its native entry semantics. Emitting a lowered
reset as another `intent(out)` call would incorrectly finalize twice.

## See Also

[Struct](../symbol_nodes/Struct.md), [ExplicitDeallocate](ExplicitDeallocate.md),
[Associate](Associate.md), [Assignment](Assignment.md)
