<!-- This is an automatically generated file. Do not edit it manually. -->
# Enum

Enum, a **symbol** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Enum(symbol_table symtab, identifier name, identifier* dependencies, identifier* members, abi abi, access access, enumtype enum_value_type, ttype type, symbol? parent)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `symtab` of type `symbol_table`, `name` of type `identifier`, `dependencies` of type `identifier*`, `members` of type `identifier*`, `abi` of type `abi`, `access` of type `access`, `enum_value_type` of type `enumtype`, `type` of type `ttype`, `parent` of type `symbol?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* The common type of EnumType cannot be `nullptr`. [...] doesn't seem to follow this rule.
* All members of EnumType must have their values to be set. [...] doesn't seem to follow this rule in [...] EnumType.
* All members of EnumType must the same type. [...] doesn't seem to follow this rule in [...] EnumType.
* Properties of enum value members don't match correspond to `Enum::m_enum_value_type`
<!-- END AUTO: restrictions -->
