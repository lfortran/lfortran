# ForEach

Iterates over the elements of a container.

## Declaration

### Syntax

```text
ForEach(expr var, expr container, stmt* body)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `var` | the variable each element is bound to. |
| `container` | the list, set or dictionary being iterated. |
| `body` | the statements run for each element. |

### Return values

None.

## Description

**ForEach** is the `for x in c` of LPython. It has no Fortran spelling: a
Fortran `do` loop always counts, and is a [DoLoop](DoLoop.md).

The iteration order is the order of the container, which for a
[Set](../type_nodes/Set.md) is unspecified.

## Examples

```clojure
(ForEach
  :var (Var
    :v (SymbolRef 1 "i")
  )
  :container (Var
    :v (SymbolRef 1 "s")
  )
  :body [
    (Assignment
      :target (Var
        :v (SymbolRef 1 "total")
      )
      :value (IntegerBinOp
        :left (Var
          :v (SymbolRef 1 "total")
        )
        :op :Add
        :right (Var
          :v (SymbolRef 1 "i")
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
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/foreach_stmt.asr
:language: clojure
```

## See Also

[DoLoop](DoLoop.md), [Set](../type_nodes/Set.md), [List](../type_nodes/List.md), [Dict](../type_nodes/Dict.md)
