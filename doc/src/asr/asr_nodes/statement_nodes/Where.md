# Where

A masked array assignment.

## Declaration

### Syntax

```text
Where(expr test, stmt* body, stmt* orelse)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `test` | the logical array mask. |
| `body` | the assignments performed where the mask is true. |
| `orelse` | the assignments performed where it is false, from `elsewhere`. |

### Return values

None.

## Description

`where` applies its assignments element by element under a mask. It is not a
conditional statement: the mask is an array, and both branches are array
assignments over the same shape.

The mask is evaluated once, before any assignment, so an assignment in the
body cannot change which elements the `elsewhere` applies to.

## Examples

```clojure
(Where
  :test (Var
    :v (SymbolRef 1 "mask")
  )
  :body [
    (Assignment
      :target (Var
        :v (SymbolRef 1 "a")
      )
      :value (IntegerConstant
        :n 0
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :overloaded nil
      :realloc_lhs false
      :move_allocation false
    )
  ]
  :orelse [
    (Assignment
      :target (Var
        :v (SymbolRef 1 "a")
      )
      :value (IntegerConstant
        :n 1
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :overloaded nil
      :realloc_lhs false
      :move_allocation false
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/where_stmt.asr
:language: clojure
```

## See Also

[If](If.md), [ForAllSingle](ForAllSingle.md), [DoConcurrentLoop](DoConcurrentLoop.md), [ArrayPack](../expression_nodes/ArrayPack.md)
