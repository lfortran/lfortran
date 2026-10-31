# DeferredMethodCall

A call of a generic type-bound procedure through a polymorphic object whose
closed generic type arguments are still deferred binders, or whose object's
declared type is the nonsealed definition of a generic derived type.

## Declaration

### Syntax

```text
DeferredMethodCall(symbol name, ttype* type_arguments, call_arg* args,
    expr dt, ttype type)
```

`name` is the generic binding (a `StructMethodDeclaration` bound to a generic
procedure whose type parameters are all closed type sets), reached through
`dt`'s declared type, which has member slots for it. `dt` is the polymorphic
object of the call and `args` are the actual arguments of the bound procedure,
including the passed object, with each closed binder replaced by a binder of
the enclosing generic definition. `type_arguments` are those enclosing
binders, in the bound procedure's binder order. `type` is the procedure's
result with the same substitution.

This expression exists only inside a checked generic definition. A generic
type's definition has no binding table, so inside its package a call through a
`class` object of the type stays deferred even with concrete type arguments,
naming the definition's binding; each instance of the type has member slots.
The template instantiator replaces it with:

- an ordinary type-bound `FunctionCall` through `dt` of the binding's member
  slot (`ASRUtils::member_slot_binding`) for the substituted concrete types,
  once `dt`'s declared type has the slots; or
- another `DeferredMethodCall`, naming the forwarding caller's binders or, in
  a generic method copied into an instance, the instance's binding.

The dynamic type of `dt` selects the procedure, so an extension's override
runs (TD-44). Code generation rejects a deferred call that has not been
instantiated.

See [TraitDeferredCall](TraitDeferredCall.md) and the "Overriding generic
bindings" section of the [traits guide](../../traits.md).
