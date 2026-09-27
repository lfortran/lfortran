# init_dispatch_phase

Which phase of the startup engine a [GlobalInitDispatch](../statement_nodes/GlobalInitDispatch.md) runs.

## Declaration

### Syntax

```text
init_dispatch_phase = InitDispatchLocal | InitDispatchCollective
```

### Values

| Value | Meaning |
|----------|-------------|
| `InitDispatchLocal` | run every startup initializer that needs no collective boundary, in stable id order. An entry point a foreign caller uses dispatches this phase. |
| `InitDispatchCollective` | run the local phase, then the collective bootstraps that initialize the coarray runtime, then the initializers that need a collective boundary, those that allocate saved coarrays or depend on one that does. Only a collective boundary dispatches it, which every image enters: a main program, or a host calling `lcompilers_initialize`. |

### Return values

None. An enumeration value is not evaluated.

## Description

A startup initializer is collective when its owner's
`global_init_collective` is set, which the `coarray` pass computes. Every
image has to allocate a saved coarray in the same order, so such an
initializer runs only in the collective phase, where every image runs the
same initializers in the same order; a local dispatch leaves it for that
phase instead.

## Examples

```clojure
(GlobalInitDispatch
  :phase :InitDispatchLocal
  :ensures [
    (SubroutineCall
      :name (SymbolRef 3 "__lcompilers_global_init_m")
      :original_name (SymbolRef 3 "__lcompilers_global_init_m")
      :args []
      :dt nil
      :strict_bounds_checking false
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/global_init.asr
:language: clojure
```

## See Also

[GlobalInitDispatch](../statement_nodes/GlobalInitDispatch.md), [Module](../symbol_nodes/Module.md)
