# String

String, a **ttype** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
String(int kind, expr? len, string_length_kind len_kind, string_physical_type physical_type)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `kind` of type `int`, `len` of type `expr?`, `len_kind` of type `string_length_kind`, `physical_type` of type `string_physical_type`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* String length must be of type INTEGER,found [...]
* String length must be length >= 0 Current length is -> [...]
* String of physical type [...] + existing length => must have length kind of "ExpressionLength".
* String of physical type [...] + non-existing length => must have length kind of "AssumedLength" OR "DeferredLength" OR "ImplicitLength".
* Implicit length kind must appear in StringPhysicalCast expression.
<!-- END AUTO: restrictions -->
