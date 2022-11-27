# If

Conditionally executes on block of statements depending on the evaluation of a
logical expression, a `stmt` node.

## Declaration

### Syntax

```fortran
If(expr test, stmt* body, stmt* orelse)
```

### Arguments

`test` contains statement or condition to be tested.
`body` contains block of 0 or more statements to be executed as per evaluation
of `test`.
`orelse` contains pointer target to `else` block.

### Return values

None.

## Description

**If** conditionally executes one block of constructs or statements depending
on the evaluation of a logical expression. If a construct name is specfied at
the beginning of an `IF THEN` statement, the same name must appear in the
corresponding `END IF` statement. If a construct name is specified on an `ELSE IF`
or `ELSE` statement, the same name must appear in the corresponding `IF THEN`
and `END IF` statements.

Depending on the evaluation of the logical expression, one block or no block is
executed.

The logical expressions are evaluated in the order in  which they appear, until
a true value is found or an `ELSE` or `END IF` statement is encountered.

Once a true value is found or an `ELSE` statement is encountered, the block
immediately following it is executed and the construct execution terminates.


## Types

expression and block of statements, including a pointer.

## Examples

```fortran
program if
implicit none
if (.false.) error stop
end
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            if_01:
                (Program
                    (SymbolTable
                        2
                        {

                        })
                    if_01
                    []
                    [(If
                        (LogicalConstant
                            .false.
                            (Logical 4 [])
                        )
                        [(ErrorStop
                            ()
                        )]
                        []
                    )]
                )

        })
    []
)

```

## See Also

[IfArithmetic](ifarithmetic.md)
