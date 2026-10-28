# storage_type

How a variable's storage behaves.

## Declaration

### Syntax

```text
storage_type = Default | Save | Parameter | Threadprivate
```

### Values

| Value | Meaning |
|----------|-------------|
| `Default` | storage lasting as long as the scope it is declared in. |
| `Save` | the `save` attribute: the variable keeps its value between calls, so it is allocated statically. |
| `Parameter` | a named constant. Its `value` is required and is substituted wherever the name is used, so it needs no storage at all. |
| `Threadprivate` | a module variable with a separate persistent instance for each thread. |

### Return values

None. An enumeration value is not evaluated.

## Description

A `Parameter` requires the [Variable](../symbol_nodes/Variable.md) to have a
folded `value`: a named constant with no value could not be used in a constant
expression.

`Threadprivate` represents an OpenMP `threadprivate` directive processed with
`--openmp`. Currently, only module variables are supported. It replaces the `Save` marker while
retaining the module variable's lifetime and declaration initializer, and
is preserved in module files. LLVM emits thread-local globals, including
imported declarations. The C and C++ backends report this storage as unsupported.

Declaration ownership is independent of lifetime. A
[Variable](../symbol_nodes/Variable.md) with `external_storage=true` refers to
storage defined elsewhere without changing its `storage` value. In particular,
importing a `BindC` threadprivate global retains both its C ABI and its
`Threadprivate` storage, so definitions and declarations agree on thread-local
addressing.

## See Also

[Variable](../symbol_nodes/Variable.md), [IntegerConstant](../expression_nodes/IntegerConstant.md)
