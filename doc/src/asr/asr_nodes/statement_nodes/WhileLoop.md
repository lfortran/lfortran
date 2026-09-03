# WhileLoop

A loop that runs while a condition holds.

## Declaration

### Syntax

```text
WhileLoop(identifier? name, expr test, stmt* body, stmt* orelse)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the construct name, or `nil`. |
| `test` | the condition, checked before each iteration. |
| `body` | the statements of the loop. |
| `orelse` | statements to run when the loop ends because the condition became false rather than through an `exit`. Fortran has no such clause; it is there for Python's `while ... else`. |

### Return values

None.

## Description

The condition is checked before the first iteration, so the body may never
run. A [DoLoop](DoLoop.md) is the counted form and knows its trip count in
advance; a **WhileLoop** does not.

## Examples

```clojure
(WhileLoop
  :name nil
  :test (IntegerCompare
    :left (Var
      :v (SymbolRef 1 "j")
    )
    :op :Lt
    :right (IntegerConstant
      :n 100
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :type (Logical
      :kind 4
    )
    :value nil
  )
  :body [
    (Assignment
      :target (Var
        :v (SymbolRef 1 "j")
      )
      :value (IntegerBinOp
        :left (Var
          :v (SymbolRef 1 "j")
        )
        :op :Add
        :right (IntegerConstant
          :n 1
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
      :overloaded nil
      :realloc_lhs false
      :move_allocation false
    )
  ]
  :orelse []
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/control_stmt.asr
:language: clojure
```

## See Also

[DoLoop](DoLoop.md), [Exit](Exit.md), [Cycle](Cycle.md)
