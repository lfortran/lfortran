# Expr

Evaluates an expression and discards its value.

## Declaration

### Syntax

```text
Expr(expr expression)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `expression` | the expression to evaluate. |

### Return values

None.

## Description

Fortran has no expression statement: a function whose result is not wanted is
called with `call` only if it is a subroutine. **Expr** exists for LPython,
where `f(x)` is a statement, and for compiler-generated code that needs an
expression evaluated for its effect.

## Examples

```clojure
(Expr
  :expression (FunctionCall
    :name (SymbolRef 1 "next")
    :original_name nil
    :args [
      (call_arg
        :value (Var
          :v (SymbolRef 2 "a")
        )
      )
    ]
    :type (Integer
      :kind 4
    )
    :value nil
    :dt nil
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/expr_stmt.asr
:language: clojure
```

## See Also

[Assignment](Assignment.md), [SubroutineCall](SubroutineCall.md), [FunctionCall](../expression_nodes/FunctionCall.md)
