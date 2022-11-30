# Where

Where, masks the evaluation of expression and assignments of values in array
assignment statements, a `stmt` node.

## Declaration

### Syntax

```fortran
Where(expr test, stmt* body, stmt* orelse)
```

### Arguments

`test` contains condition or expression to be tested.
`body` contains statement body.
`orelse` contains the else condition if `where` is not true or served.

### Return values

None.

## Description

**Where** statement masks the evaluation of expression and assignments of values
in array assignment statements. It does this according to the value of logical
array expression.

If the `where` statement is not the first statement of `where` construct, it can
be used as the terminal statement of  a `do` or `do while` construct.

## Types

Not applicable.

## Examples

```fortran
program main
  real :: x(10)
  x = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
  where (x/=5)
     x = 32.0
  elsewhere
     x = 0.0
  end where
  print *, x
end program
```

ASR:

```fortran
```

## See Also
