# StringSection

## ASR

<!-- BEGIN AUTO: asr -->
```
StringSection(expr arg, expr? start, expr? end, expr? step, ttype type, expr? value)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* StringSection start member must be provided
* StringSection end member must be provided
* StringSection step member must be provided
* StringSection return type must be a string
* StringSection's string-return node must have length expression (NOT `nullptr`)
<!-- END AUTO: verify -->
