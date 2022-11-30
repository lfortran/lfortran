# IfExp

If Expression, a `expr` ASR node.

## Declaration

### Syntax

```fortran
IfExp(expr test, expr body, expr orelse, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|--|--|
| `test`| expression to be tested |
| `body`| 0 or more statements or constructs to be executed inside `if` |
| `orelse` | construct to be executed if `if` fails |
| `type` | table entry type |
| `value` | expression |

### Return values

The return value is the expression that the IfExp represents.

## Description

**IfExp** represents If expression type. More information on `if` can be found in
[if](../statement_nodes/if.md). 

## Types

Not applicable.

## Examples

Following example code creates LFortran expression from ASR's `IntegerBinOp`:

```fortran
program if
if(1) error stop
end
```

ASR:

```fortran
(TranslationUnit
    (SymbolTable
        1
        {
            if:
                (Program
                    (SymbolTable
                        2
                        {

                        })
                    if
                    []
                    [(If
                        (IntegerConstant 1 (Integer 4 []))
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

