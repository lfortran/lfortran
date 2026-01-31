# Var

Var, a **expr** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Var(symbol v)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input argument is `v` of type `symbol`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

_No ASR example yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* `Var_t::m_v` cannot be `nullptr`
* `Var_t::m_v` [...] does not point to a `Variable_t`, `Function_t`, or `Enum_t` (possibly behind `ExternalSymbol_t`)
* `Var::m_v` `[...]` cannot point outside of its symbol table
<!-- END AUTO: restrictions -->
