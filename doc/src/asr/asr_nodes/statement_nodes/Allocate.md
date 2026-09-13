# Allocate

Allocates allocatable variables and pointers.

## Declaration

### Syntax

```text
Allocate(alloc_arg* args, expr? stat, expr? errmsg, expr? source)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `args` | one [alloc_arg](../helper_nodes/alloc_arg.md) per object being allocated, carrying the object, its shape, its codimensions, its character length and its dynamic type. |
| `stat` | a variable receiving the status: zero on success, non-zero on failure. With `stat` the program continues after a failed allocation; without it, it stops. |
| `errmsg` | a character variable receiving the error message. |
| `source` | the expression of `source=`: the new object takes its shape, and its value. |

### Return values

None.

## Description

Allocating an already allocated object is an error at run time.

The shape lives in the `dims` of each `alloc_arg`, not in the type of the
variable: an allocatable is declared with a deferred shape and gets its bounds
here. A character length is given by `len_expr`, and `sym_subclass` gives the
dynamic type when allocating a polymorphic object.

[ReAlloc](ReAlloc.md) is the related node for growing an object that may
already be allocated, and
[ExplicitDeallocate](ExplicitDeallocate.md) is its counterpart.

## Examples

```clojure
(Allocate
  :args [
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
  ]
  :stat (Var
    :v (SymbolRef 1 "stat")
  )
  :errmsg nil
  :source nil
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/allocate_stmt.asr
:language: clojure
```

## See Also

[ReAlloc](ReAlloc.md), [ExplicitDeallocate](ExplicitDeallocate.md), [ImplicitDeallocate](ImplicitDeallocate.md), [alloc_arg](../helper_nodes/alloc_arg.md), [Allocatable](../type_nodes/Allocatable.md)
