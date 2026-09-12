# IfExp

A conditional expression.

## Declaration

### Syntax

```text
IfExp(expr test, expr body, expr orelse, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `test` | the condition, of a logical type. |
| `body` | the value when the condition is true. |
| `orelse` | the value when it is false. |
| `type` | the type of the expression. Both branches have it. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The two branches have the same type, and the type of the whole expression is
theirs. **IfExp** is LPython's `a if c else b` and the Fortran 2023 conditional
expression `( test ? body : orelse )` (10.1.2.3 R1002), and it is also
convenient for compiler-generated code.

Unlike the [If](../statement_nodes/If.md) statement, both branches must
produce a value.

The Fortran front end only produces **IfExp** from an executable expression.
10.1.11 and 10.1.12 enumerate the primaries a specification expression and a
constant expression may contain, and a conditional expression is in neither
list, so it is rejected in a kind type parameter, a character length, an array
bound and an initialization expression. **IfExp** therefore never carries a
compile time value from the Fortran front end.

Only the chosen branch is evaluated. Fortran 2023 requires this
(10.1.4 NOTE 3): a function call in the branch that is not taken must not run,
so **IfExp** must never be rewritten into `merge`, which evaluates both of its
arguments. The multi-arm form of R1002,
`( c1 ? a : c2 ? b : d )`, is represented by nesting **IfExp** in the `orelse`
position.

A result that is an array, a derived type or polymorphic takes its shape,
length type parameters and dynamic type from the branch that is chosen
(10.1.4 p22-23), so there is no single descriptor a backend could write into
before the branch is known. The `conditional_expr` pass lowers those results
into an allocatable temporary and an [If](../statement_nodes/If.md) statement.
Scalars of intrinsic type, including character, reach the backends as
**IfExp** and are lowered there with a real branch.

## Examples

```clojure
(IfExp
  :test (Var
    :v (SymbolRef 1 "b")
  )
  :body (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :orelse (IntegerConstant
    :n 0
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/ifexp.asr
:language: clojure
```

## See Also

[If](../statement_nodes/If.md), [Select](../statement_nodes/Select.md)
