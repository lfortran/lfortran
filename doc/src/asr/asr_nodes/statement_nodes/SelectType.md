# SelectType

A `select type` construct: dispatch on the dynamic type of a polymorphic object.

## Declaration

### Syntax

```text
SelectType(expr selector, identifier? assoc_name, type_stmt* body,
    stmt* default)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `selector` | the polymorphic object being examined. |
| `assoc_name` | the name the selector is known by inside the branches, with the branch's type; `nil` when the selector is a name already. |
| `body` | the branches, each a [type_stmt](../helper_nodes/type_stmt.md). |
| `default` | the statements of `class default`. |

### Return values

None.

## Description

The branches are `type is`, which matches one dynamic type exactly, and
`class is`, which matches a type or anything extending it. They are
[TypeStmtName](../helper_nodes/type_stmt.md) and `ClassStmt` respectively, and
at most one runs.

Inside a branch the selector has the branch's type rather than the declared
polymorphic type, which is what makes the construct useful: a component of the
extending type can be named there and nowhere else.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/selecttype_stmt.asr
:language: clojure
```

## See Also

[type_stmt](../helper_nodes/type_stmt.md), [SelectRank](SelectRank.md), [Select](Select.md), [StructType](../type_nodes/StructType.md)
