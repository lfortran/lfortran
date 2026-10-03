# type_stmt

One branch of a `select type` construct.

## Declaration

### Syntax

```text
type_stmt
    = TypeStmtName(symbol sym, stmt* body)
    | ClassStmt(symbol sym, stmt* body)
    | TypeStmtType(ttype type, stmt* body)
```

### Arguments

None.

### Return values

None.

## Description

A branch is one of three constructors.

**TypeStmtName** is `type is (t)`, matching one dynamic type exactly:

| Argument | Description |
|----------|-------------|
| `sym` | the [Struct](../symbol_nodes/Struct.md) the branch matches. |
| `body` | the statements of the branch. |

**ClassStmt** is `class is (t)`, matching that type or anything extending it:

| Argument | Description |
|----------|-------------|
| `sym` | the type the branch matches. |
| `body` | the statements of the branch. |

**TypeStmtType** matches an intrinsic type, `type is (integer(4))`:

| Argument | Description |
|----------|-------------|
| `type` | the type the branch matches. |
| `body` | the statements of the branch. |

Inside a branch the selector has the branch's type rather than its declared
polymorphic type.

## Examples

```clojure
(TypeStmtName
  :sym (SymbolRef 4 "circle")
  :body [
    (Assignment
      :target (Var
        :v (SymbolRef 4 "x")
      )
      :value (RealConstant
        :r 1.0
        :type (Real
          :kind 4
        )
      )
      :overloaded nil
      :realloc_lhs false
      :move_allocation false
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/selecttype_stmt.asr
:language: clojure
```

## See Also

[SelectType](../statement_nodes/SelectType.md), [case_stmt](case_stmt.md), [StructType](../type_nodes/StructType.md)
