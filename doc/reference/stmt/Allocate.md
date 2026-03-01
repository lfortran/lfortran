# Allocate

## ASR

<!-- BEGIN AUTO: asr -->
```
Allocate(alloc_arg* args, expr? stat, expr? errmsg, expr? source)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* Allocate should only be called with Allocatable or Pointer type inputs, found [...]
* `Allocate::m_sym_subclass` must point to a `Struct_t` when the `m_a` member is of a type StructType
* Allocating a variable that's a string of deferred length requires providing a length to allocate with
* Allocate for arrays should have dimensions specified, found only array variable with no dimensions
<!-- END AUTO: verify -->
