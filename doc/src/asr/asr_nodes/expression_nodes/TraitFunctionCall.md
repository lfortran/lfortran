# TraitFunctionCall

Invoke an ordinary or erased generic function selected dynamically from a view.

## Declaration

### Syntax

```text
TraitFunctionCall(symbol name, int slot, call_arg* args, ttype type)
```

`name` is the normalized contract interface at `slot`, not a concrete
implementation. `args[0]` is the borrowed view; the remaining arguments have
the message's normal types, order, and association attributes. `type` is its
scalar integer result type in the initial runtime subset.
For a generic message, those remaining arguments use the verified erased slot
signature: each generic scalar is a checked `TraitPack`/view, or a
`TraitDeferredPack` within a still-generic consumer. The first view selects the
provider; the argument views independently supply nominal operation evidence.

Verification checks the slot against the actual view's canonical contract and
then applies ordinary call checks. Calls remain dynamic in ASR and serialized
modules. LLVM loads the known inline selected slot and uses the ordinary function ABI,
without conformance lookup or receiver-name inference.
