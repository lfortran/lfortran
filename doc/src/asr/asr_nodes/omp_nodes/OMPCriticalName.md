# OMPCriticalName

The name of a `critical` construct.

## Declaration

### Syntax

```text
OMPCriticalName(identifier name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the name in `critical (name)`, in lower case. |

### Return values

None.

## Description

It appears only on an [OMPRegion](../statement_nodes/OMPRegion.md) whose
region is `Critical`. Critical constructs with the same name exclude each
other, wherever they are in the program, and do not exclude critical
constructs with another name. A critical construct without this clause is
unnamed, and all unnamed critical constructs exclude each other.

Fortran names are case-insensitive, so `name` is stored in lower case.

## Examples

`!$omp critical (acc)` gives:

```clojure
(OMPCriticalName
  :name "acc"
)
```

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md),
[omp_region_type](omp_region_type.md), [omp_clause](omp_clauses.md)
