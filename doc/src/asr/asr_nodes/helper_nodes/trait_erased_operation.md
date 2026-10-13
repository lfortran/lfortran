# trait_erased_operation

A checked operation's explicit erased runtime substitution.

## Declaration

### Syntax

```text
trait_erased_operation = (symbol requirement, symbol procedure)
```

`requirement` names a normalized restriction of the original generic binder.
`procedure` names an ordinary typed wrapper owned by `TraitErasure`. The wrapper
forwards its argument view and ordinary arguments to the corresponding canonical
runtime slot. It cannot substitute provider payload storage for the argument.

The shared instantiator substitutes this procedure for the checked restriction
in the same generic body used by static specialization.
