# Var

LFortran variable.

## Declaration

### Syntax

```fortran
Var(symbol v)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|   `v`   | symbol name |

### Return values

The return value is the expression that the Var represents.

## Description

**IntegerBinOp** represents LFortran variable. It can be:

1. `INTEGER`
2. `REAL`
3. `COMPLEX`
4. `LOGICAL`
5. `CHARACTER`

A Fortran variable can be considered as a box that is capable of holding a single
value of certain type. Thus, a variable has a name, the variable name and a type.

## Types

Only accepts symbol name for LFortran variable.

## Examples

```fortran
character(len = 5), parameter :: intro = "I've "
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            intro:
                (Variable
                    1
                    intro
                    Local
                    (StringConstant
                        "I've "
                        (Character 1 5 () [])
                    )
                    (StringConstant
                        "I've "
                        (Character 1 5 () [])
                    )
                    Parameter
                    (Character 1 5 () [])
                    Source
                    Public
                    Required
                    .false.
                )

        })
    []
)

```

## See Also

