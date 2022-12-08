# Assert

Test for condition or expression to be true, a stmt node.

## Declaration

### Syntax

```fortran
Assert(expr test, expr? msg)
```

### Arguments

`test` contains the expression or condition to be tested for true.
`msg` contains the error message if the condition tested is not true and assert
fails.

### Return values

None.

## Description

**Assert** statement consists of the `assert` keyword, the expression or
conditoon to test, and an optional message. The condtion or expression being
tested is always true. If the assertion expression or conditoon is true, nothing
happens and the program unit continues to normal executions otherwise the program
unit is halted and aborted at `assert` statement, when it fails.

## Types

Expression for condtion and message to be printed.

## Examples

```fortran
program assert
implicit none
ASSERT(.true.)
end program
```

ASR:

```fortran

```

## See Also
