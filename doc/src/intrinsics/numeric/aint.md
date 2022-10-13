# aint(x, [kind]): Truncate to a Whole Number

Truncate to a whole number.

## Declaration

### Syntax

```fortran
retval = aint(x, [kind])
elemental real function aint(x, [kind])
```

### Arguments

`x` the input value must be of type real.

`kind` the optional input parameter initialises the kind of the result.

### Return values

The return value is of type integer(kind) if kind is passed as input parameter.
If not, default kind real is returned.

The return value is equal to or nearest largest whole number greater than or
equal to `x` not exceeding its magnitude.

## Description

**aint(x)** returns the nearest largest whole number greater than or equal to
`x` not exceeding input value's magnitude.

The optional parameter `kind` specifies the kind of the result.

## Types

Supported argument types is real.

## Examples

```fortran
program intrinsics_aint
	print *, aint(3.1)
	print *, floor(-3.1)
end program
```

**Result:**

```
3
-3
```

## See Also

[ceiling](ceiling.md), [mod](mod.md).
