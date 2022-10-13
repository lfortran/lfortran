# anint(x, [kind]): Round to Nearest Whole Number

Round to nearest whole number.

## Declaration

### Syntax

```fortran
retval = anint(x, [kind])
elemental real function anint(x, [kind])
```

### Arguments

`x` the input value must be of type real.

`kind` the optional input parameter initialises the kind of the result.

### Return values

The return value is of type integer(kind) if kind is passed as input parameter.
If not, default kind real is returned.

The return value is equal to rounded whole number.

## Description

**anint(x)** calculates rounded value of input parameter `x`.

If `x` is less than or equal to 0 i.e., if `x` is negative or 0, `anint(x)`
returns `aint(x - 0.5)`.

If `x` is greater than 0, `anint(x)` returns `aint(x + 0.5)`.

The optional parameter `kind` specifies the kind of the result.

## Types

Supported argument types is real.

## Examples

```fortran
program intrinsics_anint
	print *, anint(2.6)
	print *, anint(-2.6)
end program
```

**Result:**

```
3.0
-3.0
```

## See Also

[aint](aint.md).
