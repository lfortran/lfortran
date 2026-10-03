# OMPTargetRequested

The source asked for this region to run on a device.

## Declaration

### Syntax

```text
OMPTargetRequested()
```

### Arguments

None.

### Return values

None.

## Description

`!$omp target` is a request to execute a region on a device. Canonicalizing
a target construct flattens the `target` / `teams` / `distribute` nest into a
single **OMPRegion**, which would otherwise lose the fact that a device was
asked for; this clause keeps it.

The clause records what the source asked for, not what was chosen. The choice
is `exec_target` on the **OMPRegion**. When a region carrying this clause is
not given to a device, the two disagree, and the compiler reports why the
request could not be met.

## Examples

```clojure
(OMPTargetRequested)
```

## See Also

[OMPRegion](../statement_nodes/OMPRegion.md),
[exec_target](../enum_nodes/exec_target.md),
[OMPDevice](OMPDevice.md), [omp_clause](omp_clauses.md)
