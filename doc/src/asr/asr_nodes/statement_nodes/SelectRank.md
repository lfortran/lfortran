# SelectRank

A `select rank` construct: dispatch on the rank of an assumed-rank argument.

## Declaration

### Syntax

```text
SelectRank(identifier? name, expr selector, rank_stmt* body,
    stmt* default)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the construct name, or `nil`. |
| `selector` | the assumed-rank dummy argument being examined. |
| `body` | the branches, each a [rank_stmt](../helper_nodes/rank_stmt.md) matching one rank. |
| `default` | the statements of `rank default`. |

### Return values

None.

## Description

An assumed-rank dummy argument, declared `dimension(..)`, has no rank until it
is called. `select rank` is what gives it one: inside a branch the argument
has the branch's rank and can be indexed and sectioned normally.

## Examples

```clojure
(SelectRank
  :name nil
  :selector (Var
    :v (SymbolRef 2 "a")
  )
  :body [
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
  ]
  :default [
    (Assignment
      :target (Var
        :v (SymbolRef 2 "n")
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
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/selectrank_stmt.asr
:language: clojure
```

## See Also

[rank_stmt](../helper_nodes/rank_stmt.md), [SelectType](SelectType.md), [ArrayRank](../expression_nodes/ArrayRank.md)
