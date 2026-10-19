# TraitDiscover

Form a borrowed trait view of a polymorphic selector's payload with the
conformance that a matching `TraitConforms` query found.

## Declaration

### Syntax

```text
TraitDiscover(expr selector, symbol contract, symbol* witnesses, ttype type)
```

`selector`, `contract` and `witnesses` are as in `TraitConforms`. `type` is the
`TraitObjectType` of `contract`, a borrowed view. This expression has no
constant value.

## Semantics

The result copies the concrete vptr and payload address of `selector`, and the
lifecycle and method addresses of the witness that `TraitConforms` with the
same operands finds: from the runtime conformance table of the dynamic type,
or else the visible retroactive witness of exactly that type. The payload is
neither copied nor finalized, and the view owns nothing. The conformance must
exist, so the node may only be evaluated where the query is known to be true.

Semantics binds the associate name of a `CLASS IS (ITrait)` guard of SELECT
TYPE to this node. Definability of the view follows the selector, as for other
SELECT TYPE associate names.

The verifier checks the operands as for `TraitConforms`, that `type` is a view
of the queried contract, and that the block containing the node is entered only
under a `TraitConforms` with the same operands.

## See Also

[Experimental traits](../../traits.md), [TraitConforms](TraitConforms.md),
[TraitInspect](TraitInspect.md).
