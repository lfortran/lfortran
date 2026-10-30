# ProcedureType

The physical representation of a procedure value, not its definition.

## Declaration

### Syntax

```text
ProcedureType(ttype signature, procedure_representation representation)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `signature` | the logical `FunctionType`, including arguments added by ABI passes. |
| `representation` | [RawPointer or Closure](../enum_nodes/procedure_representation.md). |

## Description

`closure_lowering` gives ordinary Fortran values two words: code and
environment. Null environment selects the native ABI; otherwise code names
an environment-first entry. Both words are null for disassociated values.
Copies preserve both words, and association compares both. Raw interoperable
C pointers retain their one-word representation.
Local Fortran procedure pointers store the complete pair; pointer association
does not add an extra indirection to that storage.

Procedure-valued arguments and results nested in `signature` are represented
recursively. Their convention follows that signature's ABI: `BIND(C)` uses raw
pointers, while Fortran uses closures, even when the outer value is stored with
a different representation. A definition's signature and its owned dummy and
result declarations must agree on these represented callable types.

A module procedure used as a value retains the same callable array ABI in its
interface and its implementation, including implementations in descendant
submodules.

Definitions retain `FunctionType`. The inspection printer adds the internal
attribute `closure` or `raw_pointer` to the procedure declaration.
