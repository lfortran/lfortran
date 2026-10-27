# Reference

The address of a variable, held by a member of a host frame.

## Declaration

### Syntax

```text
Reference(ttype type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `type` | the type of the variable whose address the member holds. |

### Return values

None. A type is not evaluated.

## Description

A **Reference** is the type of a member of a frame, the derived type that the
`nested_vars` pass declares in a host so that its internal procedures reach
the variables of the instance of the host that invoked them (see "Host
Association" in the design document). It is the type of such a member and of
nothing else: the verifier reports a Reference anywhere else, including inside
another type, as `asr.verify.reference.member_only`.

[BindReference](../statement_nodes/BindReference.md) stores the address of a
variable in the member. A
[StructInstanceMember](../expression_nodes/StructInstanceMember.md) of the
member then designates that variable itself and has type `type`, the type of
the variable, as `asr.verify.reference.designator_type` checks. It is the
`symbolic_value` of a variable of `HostAssociated`
[storage](../enum_nodes/storage_type.md), which stands for the variable in an
internal procedure.

Unlike a [Pointer](Pointer.md), a member of Reference type has no association
status and no value of its own, and nothing allocates, deallocates,
finalizes, copies or default-initializes it with the object that contains it:
generic code that does not know the type leaves it alone. A backend lays it
out as the address of the storage of a variable of type `type`.

## Examples

```clojure
(Reference
  :type (Integer
    :kind 4
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/host_frame.asr
:language: clojure
```

## See Also

[BindReference](../statement_nodes/BindReference.md), [storage_type](../enum_nodes/storage_type.md), [Pointer](Pointer.md), [StructType](StructType.md)
