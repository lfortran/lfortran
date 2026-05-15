# Enum

## ASR

<!-- BEGIN AUTO: asr -->
```
Enum(symbol_table symtab, identifier name, identifier* dependencies, identifier* members, abi abi, access access, enumtype enum_value_type, ttype type, symbol? parent)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* The common type of EnumType cannot be `nullptr`. [...] doesn't seem to follow this rule.
* All members of EnumType must have their values to be set. [...] doesn't seem to follow this rule in [...] EnumType.
* All members of EnumType must the same type. [...] doesn't seem to follow this rule in [...] EnumType.
* Properties of enum value members don't match correspond to `Enum::m_enum_value_type`
<!-- END AUTO: verify -->
