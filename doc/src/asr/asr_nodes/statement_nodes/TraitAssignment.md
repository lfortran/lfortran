# TraitAssignment

Intrinsic scalar owning value assignment with explicit snapshot ordering.

## Declaration

### Syntax

```text
TraitAssignment(expr target, expr value, symbol? witness)
```

`target` is a scalar allocatable trait variable or a definable owning component.
`value` is either an exact
nonpolymorphic concrete value with selected `witness`, or a same-contract
borrowed view carrying its witness. Conformance is never reselected for an
already formed view.

The operation first evaluates and structurally initialize-copies the complete
RHS into a unique fresh snapshot, without defined assignment or FINAL.
Only then may it finalize/destroy the old LHS value.
Equal nominal dynamic types retain their outer allocation; unequal types
replace it. Component-defined assignment uses the actual prepared/live LHS,
including its old value for INTENT(INOUT) components. The snapshot's owned
storage is then released without invoking user finalizers. Both branches replace the
selected witness, including equal-type assignments between distinct conformances.

This ordering supports self-assignment and RHS expressions that read the LHS.
Allocatable components copy independently and pointer components preserve
association. Borrowed views are not assignment targets. Ordinary `Assignment`
or `Associate` of a trait header is rejected by verification because neither
expresses this ownership protocol.

This describes direct assignment to the owner slot. When an ordinary containing
derived object is assigned instead, its noncoarray allocatable components are
recreated before component assignment, following Fortran 2023 10.2.1.3.
