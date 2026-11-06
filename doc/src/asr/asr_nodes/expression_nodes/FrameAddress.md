# FrameAddress

The address of the host frame that a frame entry is passed with.

## Declaration

### Syntax

```text
FrameAddress(symbol procedure, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `procedure` | a frame entry: an internal procedure, visible here, whose last dummy argument is a `type(c_ptr)` value dummy. |
| `type` | `type(c_ptr)`, a [CPtr](../type_nodes/CPtr.md). |
| `value` | always `nil`. |

### Return values

The address of the frame of the instance of the host of `procedure` that
evaluates the expression, or a null pointer when `procedure` needs no frame.

## Description

With `--fast`, the `specialize_frame_arguments` pass copies a procedure whose
dummy procedures are only called or passed on. In the copy, each such dummy
procedure is a pair of dummies: a code pointer whose interface has an extra
last `type(c_ptr)` value dummy, and the address of a frame. An actual
procedure is passed to the pair as a frame entry, an internal procedure that
calls it, and a **FrameAddress** of the entry.

The `nested_vars` pass gives a frame entry the frame of its host through that
last dummy instead of a frame dummy, so a procedure value of the entry is its
plain code address and needs no thunk, and it replaces every **FrameAddress**:
by a [PointerToCPtr](PointerToCPtr.md) of the frame object or the frame dummy
that a call of the entry from the same place would pass, or by a
[PointerNullConstant](PointerNullConstant.md) when the entry needs no frame.
It is not Fortran: `--show-fortran` prints it as `frame_address(procedure)`.
No **FrameAddress** remains after `nested_vars`
(`asr.verify.nested_vars.frame_address`). A `procedure` that is not a frame
entry is reported as `asr.verify.frame_address.procedure`.

## Examples

The copy of the recursive `a` in Knuth's man or boy test passes its internal
procedure `b` with the frame of the instance:

```clojure
(FrameAddress
  :procedure (SymbolRef 4 "b__lcompilers_frame_entry")
  :type (CPtr
    :kind CPointer
  )
  :value nil
)
```

## See Also

[BindReference](../statement_nodes/BindReference.md), [Reference](../type_nodes/Reference.md), [PointerToCPtr](PointerToCPtr.md), [CPtr](../type_nodes/CPtr.md)
