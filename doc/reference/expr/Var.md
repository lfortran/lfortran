<!-- This is an automatically generated file. Do not edit it manually. -->

# Var

Var, a **expr** node.

## Declaration

### Syntax

Var(symbol v)

### Arguments
Input argument is `v` of type symbol.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* Var_t::m_v cannot be nullptr
* Var_t::m_vdoes not point to a Variable_t,Function_t, or Enum_t (possibly behind ExternalSymbol_t)
* Var::m_v `` cannot point outside of its symbol table