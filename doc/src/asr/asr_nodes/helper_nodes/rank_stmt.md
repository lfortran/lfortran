# rank_stmt

One branch of a `select rank` construct.

## Declaration

### Syntax

```text
rank_stmt = RankExpr(expr rank, stmt* body)
```

### Arguments

None.

### Return values

None.

## Description

There is one constructor, **RankExpr**:

| Argument | Description |
|----------|-------------|
| `rank` | the rank this branch matches. |
| `body` | the statements of the branch. |

Inside the branch the assumed-rank selector has that rank, so it can be
indexed and sectioned.

## Examples

```clojure
(RankExpr
  :rank (IntegerConstant
    :n 1
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :body [
    (Assignment
      :target (Var
        :v (SymbolRef 2 "n")
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

```{literalinclude} ../../examples/selectrank_stmt.asr
:language: clojure
```

## See Also

[SelectRank](../statement_nodes/SelectRank.md), [ArrayRank](../expression_nodes/ArrayRank.md), [case_stmt](case_stmt.md)
