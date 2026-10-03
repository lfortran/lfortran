# FunctionParam

A reference to a dummy argument from inside a signature.

## Declaration

### Syntax

```text
FunctionParam(int param_number, ttype type, expr? value)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `param_number` | the position of the argument, counting from zero. |
| `type` | the type of the expression. |
| `value` | the compile time value of the expression, when the frontend could fold it; `nil` otherwise. |

### Return values

The value of the expression.

## Description

The declaration `real :: a(n)` in a procedure whose first dummy argument is
`n` gives the array a bound that depends on another argument. In the
[FunctionType](../type_nodes/FunctionType.md) there are no symbols to point
at, so the bound refers to the argument by position instead, with a
**FunctionParam**.

It is only meaningful inside a signature. In the body of the procedure the
same value is an ordinary [Var](Var.md).

## Examples

```clojure
(FunctionParam
  :param_number 0
  :type (Integer
    :kind 4
  )
  :value nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/functionparam.asr
:language: clojure
```

## See Also

[FunctionType](../type_nodes/FunctionType.md), [Function](../symbol_nodes/Function.md), [Var](Var.md)
