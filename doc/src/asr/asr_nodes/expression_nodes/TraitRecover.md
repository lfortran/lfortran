# TraitRecover

The concrete value that erased storage holds, inside a dictionary adapter.

## Declaration

### Syntax

```text
TraitRecover(expr value, symbol dictionary, ttype type)
```

`value` is an erased variable of a dictionary adapter: an argument, the
receiver or the result. `dictionary` is the `TraitDictionary` that owns the
adapter and `type` the type of its implementation. The expression designates
the same storage as that type, so it can be passed to the binding, also as an
`intent(out)` actual of a result lowered to a dummy, and assigned to; it is
definable exactly when `value` is.

It appears only in the adapters of the dictionary it names, the one place
where the concrete type of erased storage is known.
