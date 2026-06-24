# Var

## ASR

<!-- BEGIN AUTO: asr -->
```
Var(symbol v)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* `Var_t::m_v` cannot be `nullptr`
* `Var_t::m_v` [...] does not point to a `Variable_t`, `Function_t`, or `Enum_t` (possibly behind `ExternalSymbol_t`)
* `Var::m_v` `[...]` cannot point outside of its symbol table
<!-- END AUTO: verify -->
