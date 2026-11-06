# BindReference

Makes a member of a host frame refer to a variable.

## Declaration

### Syntax

```text
BindReference(expr reference, expr variable)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `reference` | a [StructInstanceMember](../expression_nodes/StructInstanceMember.md) of a member of [Reference](../type_nodes/Reference.md) type of a frame object. |
| `variable` | the variable it refers to: a [Var](../expression_nodes/Var.md) of a variable, or a StructInstanceMember of a member of Reference type, which designates the variable that member refers to. |

### Return values

None.

## Description

**BindReference** stores the address of `variable` in the member: nothing is
copied. A host binds every member of its frame at the start of its body, so
that the internal procedures it calls with the frame reach the variables of
this instance of the host. The second form of `variable` passes a reference
on from the frame of a host to the frame of an internal procedure that is
itself a host.

The type of `variable` is the referenced type, including whether it is
allocatable or a pointer and the rank and physical type of an array
(`asr.verify.bind_reference.type`); a `variable` that is not a variable is
reported as `asr.verify.bind_reference.variable`.

## Examples

```clojure
(BindReference
  :reference (StructInstanceMember
    :v (Var
      :v (SymbolRef 2 "f")
    )
    :m (SymbolRef 2 "1_frame_n")
    :type (Integer
      :kind 4
    )
    :value nil
  )
  :variable (Var
    :v (SymbolRef 2 "n")
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/host_frame.asr
:language: clojure
```

## See Also

[Reference](../type_nodes/Reference.md), [Associate](Associate.md), [storage_type](../enum_nodes/storage_type.md)
