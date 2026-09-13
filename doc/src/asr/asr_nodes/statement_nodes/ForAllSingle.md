# ForAllSingle

A `forall` statement with a single assignment.

## Declaration

### Syntax

```text
ForAllSingle(do_loop_head head, stmt assign_stmt)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `head` | the [do_loop_head](../helper_nodes/do_loop_head.md) giving the index and its range. |
| `assign_stmt` | the assignment performed for every index value. |

### Return values

None.

## Description

`forall` differs from a loop in its evaluation order: every right hand side is
evaluated for all index values before any assignment happens, so an assignment
cannot see the values written by another index.

A `forall` construct with several statements is lowered to a sequence of
**ForAllSingle** nodes, one per assignment, which preserves that ordering
because each statement completes for all indices before the next begins.

## Examples

```clojure
(ForAllSingle
  :head (do_loop_head
    :v (Var
      :v (SymbolRef 1 "i")
    )
    :start (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :end (IntegerConstant
      :n 3
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :increment (IntegerConstant
      :n 1
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  )
  :assign_stmt (Assignment
    :target (ArrayItem
      :v (Var
        :v (SymbolRef 1 "a")
      )
      :args [
        (array_index
          :left nil
          :right (Var
            :v (SymbolRef 1 "i")
          )
          :step nil
        )
      ]
      :type (Integer
        :kind 4
      )
      :storage_format :ColMajor
      :value nil
    )
    :value (Var
      :v (SymbolRef 1 "i")
    )
    :overloaded nil
    :realloc_lhs false
    :move_allocation false
  )
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/forallsingle_stmt.asr
:language: clojure
```

## See Also

[DoConcurrentLoop](DoConcurrentLoop.md), [Where](Where.md), [do_loop_head](../helper_nodes/do_loop_head.md)
