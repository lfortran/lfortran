# TraitFunctionCall

Invoke an ordinary function selected dynamically from a view's witness.

## Declaration

### Syntax

```text
TraitFunctionCall(symbol name, int slot, call_arg* args, ttype type)
```

`name` is the normalized contract interface at `slot`, not a concrete
implementation. `args[0]` is the borrowed view; the remaining arguments have
the message's normal types, order, and association attributes. `type` is its
scalar integer result type in the initial runtime subset.

Verification checks the slot against the actual view's canonical contract and
then applies ordinary call checks. Calls remain dynamic in ASR and serialized
modules. LLVM loads the known inline selected slot and uses the ordinary function ABI,
without conformance lookup or receiver-name inference.
