# TraitObjectType

A runtime view of a nominal universal trait, distinct from concrete storage and
ordinary implementation inheritance.

## Declaration

### Syntax

```text
TraitObjectType(symbol contract)
```

`contract` references the canonical `TraitRuntimeContract`, through a visible
symbol. Its slot interfaces describe all ordinary arguments and results.
The current implementation permits only required, nonpointer, nonallocatable,
scalar `intent(in)` dummies. `TraitPack` creates compiler-borrowed views; forwarding
uses the original variable. Neither operation owns or copies the payload.

The LLVM representation carries concrete CLASS metadata, a payload address, and
an independent selected witness. It is compiler-private, not a public ABI.
Type-set traits cannot form views. Generic-method universal traits remain a
future runtime implementation stage, not a permanently excluded trait category.

See the [traits guide](../../traits.md).
