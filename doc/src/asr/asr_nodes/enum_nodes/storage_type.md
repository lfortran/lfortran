# storage_type

How a variable's storage behaves.

## Declaration

### Syntax

```text
storage_type = Default | Save | Parameter | Threadprivate | ResultArgument | HostAssociated
```

### Values

| Value | Meaning |
|----------|-------------|
| `Default` | storage lasting as long as the scope it is declared in. |
| `Save` | the `save` attribute: the variable keeps its value between calls, so it is allocated statically. |
| `Parameter` | a named constant. Its `value` is required and is substituted wherever the name is used, so it needs no storage at all. |
| `Threadprivate` | a module variable with a separate persistent instance for each thread. |
| `ResultArgument` | caller-provided storage for a function result lowered to an output dummy argument. |
| `HostAssociated` | no storage of its own: the variable of the host that its `symbolic_value` designates. |

### Return values

None. An enumeration value is not evaluated.

## Description

A `Parameter` requires the [Variable](../symbol_nodes/Variable.md) to have a
folded `value`: a named constant with no value could not be used in a constant
expression.

`Threadprivate` represents an OpenMP `threadprivate` directive processed with
`--openmp`. Currently, only module variables are supported. It replaces the `Save` marker while
retaining the module variable's lifetime and declaration initializer, and
is preserved in module files. LLVM emits thread-local globals, including
imported declarations. The C and C++ backends report this storage as unsupported.

`ResultArgument` is recorded when return-value lowering converts a result
variable into a dummy argument. It preserves the result's provenance through
symbol copying, rather than inferring it from argument position or shape.
Verification requires the variable to be a dummy of a procedure with no
`return_var`, and its intent to be `Out` or `InOut`. Alias analysis must not
give this caller-provided result storage the ordinary dummy-argument noalias
promise; it can overlap other arguments through result temporaries.

`HostAssociated` is given by the `nested_vars` pass to a local variable of an
internal procedure that stands for a variable of its host (see "Host
Association" in the design document). Its `symbolic_value` is a
[StructInstanceMember](../expression_nodes/StructInstanceMember.md) of a
member of [Reference](../type_nodes/Reference.md) type of the frame dummy
argument of its own procedure (in a thunk that a procedure value of an
internal procedure names, of the frame slot of its host: a `Save` pointer to
the frame), and its type is that of the designated variable. It is bound when the procedure is entered, before any other
declaration is evaluated, like a dummy argument associated by reference, so
it may have a type otherwise allowed only for a dummy argument. Nothing
allocates, initializes, finalizes or deallocates it: the host owns the
storage. The verifier requires such a variable to be a local variable of a
procedure, without a `value`, and not a dummy argument.

## See Also

[Variable](../symbol_nodes/Variable.md), [IntegerConstant](../expression_nodes/IntegerConstant.md)
