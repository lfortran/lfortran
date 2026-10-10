# TraitDeferredCall

A runtime call whose closed generic type arguments are still deferred binders.

## Declaration

### Syntax

```text
TraitDeferredCall(int family, ttype* type_arguments, call_arg* args,
    ttype type)
```

`args[0]` is the borrowed view that selects the provider; the remaining
arguments have the message's types with each closed binder replaced by a
binder of the enclosing generic definition. `family` is the first slot of the
message's member family in the view's canonical contract. `type_arguments` are
those enclosing binders, in the message's binder order; each must be
constrained by exactly the same type-set trait as the message binder. `type` is
the message result with the same substitution.

This expression exists only inside a checked generic definition. The existing
template instantiator replaces it with:

- a `TraitFunctionCall` of the member slot whose `type_arguments` equal the
  substituted concrete types, with ordinary argument association; or
- another `TraitDeferredCall` naming the forwarding caller's binders.

The provider stays dynamic and the member is chosen statically: no provider body
is specialized by the client. Code generation rejects a deferred call that has
not been instantiated.

See [TraitFunctionCall](TraitFunctionCall.md) and
[trait_slot](../helper_nodes/trait_slot.md).
