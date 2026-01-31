<!-- This is an automatically generated file. Do not edit it manually. -->
# StringSection

StringSection, a **expression (expr)** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
StringSection(expr arg, expr? start, expr? end, expr? step, ttype type, expr? value)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `arg` of type `expr`, `start` of type `expr?`, `end` of type `expr?`, `step` of type `expr?`, `type` of type `ttype`, `value` of type `expr?`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## ASR

<!-- Generate ASR using pickle. -->

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* StringSection start member must be provided
* StringSection end member must be provided
* StringSection step member must be provided
* StringSection return type must be a string
* StringSection's string-return node must have length expression (NOT `nullptr`)
<!-- END AUTO: restrictions -->
