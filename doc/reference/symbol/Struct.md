<!-- This is an automatically generated file. Do not edit it manually. -->

# Struct

Struct, a **symbol** node.

## Declaration

### Syntax

Struct(symbol_table symtab, identifier name, identifier* dependencies, identifier* members, identifier* member_functions, abi abi, access access, bool is_packed, bool is_abstract, call_arg* initializers, expr? alignment, symbol? parent)

### Arguments
Input arguments are `symtab` of type symbol_table, `name` of type identifier, `dependencies` of type identifier*, `members` of type identifier*, `member_functions` of type identifier*, `abi` of type abi, `access` of type access, `is_packed` of type bool, `is_abstract` of type bool, `initializers` of type call_arg*, `alignment` of type expr?, `parent` of type symbol?.

### Return values

None.

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- Generated from asr_verify.cpp. -->
* Alignmentis not a positive power of 2.