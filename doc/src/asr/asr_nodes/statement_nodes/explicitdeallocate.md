# ExplicitDeallocate

Deallocates if allocated otherwise throws a runtime error, a `stmt` ASR node.

## Declaration

### Syntax

```fortran
ExplicitDeallocate(symbol* vars)
```

### Arguments

`vars` contains pointer target.

### Return values

None.

## Description

**ExplicitDeallocate** frees the storage allocated for allocatable variables and
nonprocedure pointer targets. It also disassociates pointer. It is done by
`Deallocate()` statement. If not allocated, it throws a runtime error.

## Types

Name of variable, must be a pointer or allocatable variable.

## Examples

```fortran
```

ASR:

```fortran
```

## See Also
