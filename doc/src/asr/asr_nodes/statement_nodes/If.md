# If

A conditional statement.

## Declaration

### Syntax

```text
If(identifier? name, expr test, stmt* body, stmt* orelse)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `name` | the construct name, or `nil`. |
| `test` | the condition, of a logical type. |
| `body` | the statements run when the condition is true. |
| `orelse` | the statements run when it is false. `else if` is an **If** nested in `orelse`. |

### Return values

None.

## Description

`test` must already be logical: ASR has no notion of a value being truthy, so
a frontend that allows one inserts the comparison itself.

There is no `else if` node. `else if` is an **If** whose parent's `orelse`
holds exactly it, which keeps the tree uniform for passes that rewrite
conditionals.

## Examples

```clojure
(If
  :name nil
  :test (IntegerCompare
    :left (Var
      :v (SymbolRef 1 "i")
    )
    :op :Eq
    :right (IntegerConstant
      :n 5
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
    (Cycle
      :stmt_name "loop"
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

[IfExp](../expression_nodes/IfExp.md), [Select](Select.md), [Where](Where.md), [IfArithmetic](IfArithmetic.md)
