<!-- This is an automatically generated file. Do not edit it manually. -->
# Allocate

Allocate, a **statement (stmt)** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Allocate(alloc_arg* args, expr? stat, expr? errmsg, expr? source)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `args` of type `alloc_arg*`, `stat` of type `expr?`, `errmsg` of type `expr?`, `source` of type `expr?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Allocate should only be called with Allocatable or Pointer type inputs, found [...]
* `Allocate::m_sym_subclass` must point to a `Struct_t` when the `m_a` member is of a type StructType
* Allocating a variable that's a string of deferred length requires providing a length to allocate with
* Allocate for arrays should have dimensions specified, found only array variable with no dimensions
<!-- END AUTO: restrictions -->
