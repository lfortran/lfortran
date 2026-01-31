# String

## ASR

<!-- BEGIN AUTO: asr -->
```
String(int kind, expr? len, string_length_kind len_kind, string_physical_type physical_type)
```
<!-- END AUTO: asr -->

## Documentation

_No documentation yet._

## Verify

<!-- BEGIN AUTO: verify -->
* String length must be of type INTEGER,found [...]
* String length must be length >= 0 Current length is -> [...]
* String of physical type [...] + existing length => must have length kind of `ExpressionLength`.
* String of physical type [...] + non-existing length => must have length kind of `AssumedLength` OR `DeferredLength` OR `ImplicitLength`.
* Implicit length kind must appear in StringPhysicalCast expression.
<!-- END AUTO: verify -->
