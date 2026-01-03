# TypeInquiry

An **expr** node, which is used to represent the `Inquiry` intrinsic function
whose value is computed based on the argument type.

## Declaration

### Syntax

```fortran
TypeInquiry(int inquiry_id, ttype arg_type, expr? arg, ttype type, expr value)
```

### Arguments


| Argument Name | Argument Description |
|---------------|----------------------|
| `inquiry_id`  | Function unique ID   |
| `arg_type`    | argument type        |
| `arg`         | argument passed      |
| `type`        | output type          |
| `value`       | compile time value   |

### Return value

The return value is the expression that the TypeInquiry represents.

## Description

**TypeInquiry** is used to represent the `Inquiry` function. This node accepts
exactly one `ttype` as it's only argument (`arg_type`), and it returns a value
depending on `inquiry_id` of `ttype` `type`. Since the type `arg_type` is known
at compile time, also the compile time value is always present in `value`.
There is an optional argument `arg` that specifies the variable used in the
frontend language (used in LFortran, but not present in LPython/NumPy).

Here a list of inquiry_id that we support so far

| Inquiry function |          Output          | Output type |
|------------------|--------------------------|-------------|
| `Epsilon`        | smallest number E        | `arg_type`  |
| `Huge`           | largest number           | `arg_type`  |
| `Precision`      | decimal precision        | `int32`     |
| `Radix`          | base of a numeric model  | `int32`     |
| `Range`          | decimal exponent range   | `int32`     |
| `Rank`           | rank of a data object    | `int32`     |
| `Tiny`           | smallest positive number | `arg_type`  |

> Note: All the functions output are computed based on the `arg_type`

## Types

The `arg_type` and `type` vary for each inquiry_id signature. The output is
either the default integer or the same as the argument type.

## Examples

The following example code creates `TypeInquiry` ASR node:

```fortran
print *, tiny(5.0)
```

ASR:

```clojure
(Print
    [(TypeInquiry
        Tiny
        (Real 4)
        (RealConstant
            5.000000
            (Real 4)
        )
        (Real 4)
        (RealConstant
            0.000000
            (Real 4)
        )
    )]
    ()
    ()
)
```

## See Also

[IntrinsicFunction](IntrinsicFunction.md)
