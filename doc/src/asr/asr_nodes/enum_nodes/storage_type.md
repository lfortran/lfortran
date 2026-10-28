# storage_type

How a variable's storage behaves.

## Declaration

### Syntax

```text
storage_type = Default | Save | Parameter | Threadprivate | ExternalStorage
```

### Values

| Value | Meaning |
|----------|-------------|
| `Default` | storage lasting as long as the scope it is declared in. |
| `Save` | the `save` attribute: the variable keeps its value between calls, so it is allocated statically. |
| `Parameter` | a named constant. Its `value` is required and is substituted wherever the name is used, so it needs no storage at all. |
| `Threadprivate` | a module variable with a separate persistent instance for each thread. |
| `ExternalStorage` | a global declaration whose storage is defined in another translation unit, independently of its ABI. |

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

`ExternalStorage` refers to existing storage without allocating or initializing
it again. It preserves the variable's ABI, binding label and type. For example,
a `BindC` global defined in an earlier interactive cell retains its C name and
layout when a later cell declares it. Local character descriptors may provide
views of that external C storage without redefining it.

This state is restricted to module or translation-unit variables with `Local`
intent and no `common_block`; dummy arguments, derived-type components and
COMMON members do not have independent global storage to declare this way.

## See Also

[Variable](../symbol_nodes/Variable.md), [IntegerConstant](../expression_nodes/IntegerConstant.md)
