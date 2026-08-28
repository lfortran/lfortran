# OMPNumTeams

`num_teams`: how many teams a league has.

## Declaration

### Syntax

```text
OMPNumTeams(expr num_teams)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `num_teams` | the number of teams requested. |

### Return values

None.

## Description

It applies to a `teams` region, which creates a league of teams on a device.

## Examples

```clojure
(OMPNumTeams
  :num_teams (IntegerConstant
    :n 2
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/omp_region.asr
:language: clojure
```

## See Also

[OMPThreadLimit](OMPThreadLimit.md), [OMPDevice](OMPDevice.md), [OMPRegion](../statement_nodes/OMPRegion.md), [omp_clause](omp_clauses.md)
