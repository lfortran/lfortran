# TraitReceiver

Recover the concrete borrowed payload authorized by a selected witness.

## Declaration

### Syntax

```text
TraitReceiver(expr view, symbol witness, symbol type_declaration, ttype type)
```

The verifier requires this expression to appear in one of `witness`'s own
adapters and to use that adapter's view argument. `type_declaration` must be
the implementing nominal concrete type; `type` describes its nonpolymorphic
scalar storage. Recovery does not create a local value, ownership, or a new
conformance choice.

The expression is directly addressable. Ordinary argument-temporary passes
must not replace it with an owning copy. Normal call lowering can wrap it as
an ordinary concrete CLASS receiver when the implementation requires one.
This is not a source-level unchecked cast or trait-discovery operation.
