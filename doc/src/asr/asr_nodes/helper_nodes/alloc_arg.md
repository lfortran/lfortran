# alloc_arg

One object of an `allocate` statement, with everything needed to allocate it.

## Declaration

### Syntax

```text
alloc_arg
    = (expr a, dimension* dims, codimension* codims, expr? len_expr, symbol? sym_subclass, ttype? type)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `a` | the object being allocated. |
| `dims` | the shape it is given, one [dimension](dimension.md) per rank. |
| `codims` | the codimensions it is given, for a coarray. |
| `len_expr` | the character length, for `character(len=n) ::`. |
| `sym_subclass` | the dynamic type to allocate, for `allocate(t :: x)` with a polymorphic object. |
| `type` | the type to allocate, when it differs from the declared type. |

### Return values

None.

## Description

The shape is here rather than in the type of the variable, because an
allocatable is declared with a deferred shape and gets its bounds at each
allocation. Everything a backend needs for one object is in one product, so
[Allocate](../statement_nodes/Allocate.md) is a list of them.

## Examples

```clojure
(alloc_arg
  :a (Var
    :v (SymbolRef 1 "a")
  )
  :dims [
    (dimension
      :start (IntegerConstant
        :n 1
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :length (IntegerConstant
        :n 10
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
    )
  ]
  :codims []
  :len_expr nil
  :sym_subclass nil
  :type nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/allocate_stmt.asr
:language: clojure
```

## See Also

[Allocate](../statement_nodes/Allocate.md), [ReAlloc](../statement_nodes/ReAlloc.md), [dimension](dimension.md), [Allocatable](../type_nodes/Allocatable.md)
