# Select

Select, a **stmt** node.

## Declaration

### Syntax

<!-- BEGIN AUTO: syntax -->
```
Select(identifier? name, expr test, case_stmt* body, stmt* default, bool enable_fall_through)
```
<!-- END AUTO: syntax -->

### Arguments

<!-- BEGIN AUTO: arguments -->
Input arguments are `name` of type `identifier?`, `test` of type `expr`, `body` of type `case_stmt*`, `default` of type `stmt*`, `enable_fall_through` of type `bool`.
<!-- END AUTO: arguments -->

### Return values

None.

## Documentation

_No documentation yet._

## Restrictions

<!-- BEGIN AUTO: restrictions -->
* Select_t::m_enable_fall_through should be [...]
<!-- END AUTO: restrictions -->
