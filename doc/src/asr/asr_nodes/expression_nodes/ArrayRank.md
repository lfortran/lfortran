# ArrayRank

Rank of array.

## Declaration

### Syntax

```fortran
ArrayRank(expr v, ttype type, expr? value)
```

### Arguments

| Argument Name | Argument Description |
|---------------|----------------------|
|`v` | expression |
|`type` | table entry type |
|`value` | expression value |

### Return values

The return value is the expression that the ArrayRank represents.

## Description

**ArrayRank** represents the rank (number of dimensions) of an array expression.

- At *compile-time*, the rank is determined from the declared shape of the array. It can be used for all arrays except assumed-rank arrays where the rank is determined at runtime. It is stored in the `value` argument of the ASR Node.
- At *runtime*, the rank is determined for assumed-rank arrays (`dimension(..)`) where the rank is not known at compile-time. It is determined at runtime using the descriptor of the array.

## Usage

## Compile-Time Usage

```fortran
integer :: a(3)
integer :: r
r = rank(a)
```

ASR Node for the above example:

```clojure
(ArrayRank
    (Var 2 a)
    (Integer 4)
    (IntegerConstant 1 (Integer 4) Decimal)
)
```

Here the `value` argument of the ASR Node is `1` because the rank of array `a` is known at compile-time and it is `1`.

## Runtime Usage

```fortran
integer :: a(..)
integer :: r
r = rank(a)
```

ASR Node for the above example:

```clojure
(ArrayRank
    (Var 2 a)
    (Integer 4)
    ()
)
```
Here the `value` argument of the ASR Node is empty because the rank of array `a` is not known at compile-time and it will be determined at runtime.

