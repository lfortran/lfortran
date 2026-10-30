# procedure_representation

The physical representation of a procedure value.

## Declaration

### Syntax

```text
procedure_representation = RawPointer | Closure
```

### Values

| Value | Meaning |
|-------|---------|
| `RawPointer` | a native code pointer, including interoperable C pointers. |
| `Closure` | code and environment; null environment selects native calls, otherwise environment-first entry calls. |

Definitions retain `FunctionType`; this enumeration belongs to
[ProcedureType](../type_nodes/ProcedureType.md).
