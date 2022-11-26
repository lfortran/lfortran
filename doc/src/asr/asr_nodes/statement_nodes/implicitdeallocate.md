# ImplicitDeallocate

Deallocates if allocated otherwise does nothing, a `stmt` ASR node.

## Declaration

### Syntax

```fortran
ImplicitDeallocate(symbol* vars)
```

### Arguments

`vars` contains pointer target.

### Return values

None.

## Description

**ImplicitDeallocate** frees the storage allocated for allocatable variables and
nonprocedure pointer targets. It also disassociates pointer. It is done by
`Deallocate()` statement. If not allocated, it does nothing.

## Types

Name of variable, must be a pointer or allocatable variable.

## Examples

```fortran
```

ASR:

```fortran
```

## See Also
[explicitdeallocate](explicitdeallocate.md)
