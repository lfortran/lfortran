# Requirement

## Declaration

### Syntax

### Arguments

### Return values

## Description

## Types

## Examples

```
module semigroup_m
    requirement semigroup(T, op)
        type, deferred :: T
        function op(x,y) result(z)
            type(T), intent(in) :: x, y
            type(T) :: z
        end function
    end requirement
end module
```

## See Also

* [Symbols](symbol.md)

* [Template](Template.md)

* [Type Parameter](../type_nodes/TypeParameter.md)