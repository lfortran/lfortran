# Union

A union: several members sharing one storage location.

## Declaration

### Syntax

```text
Union(symbol_table symtab, identifier name, identifier* dependencies,
    identifier* members, abi abi, access access,
    call_arg* initializers, symbol? parent)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the union, with one [Variable](Variable.md) per member. |
| `name` | the name of the union. |
| `dependencies` | the names of the symbols the definition refers to. |
| `members` | the names of the members, in declaration order. |
| `abi` | `Source`, or `BindC` for a union declared in C. |
| `access` | `Public` or `Private`. |
| `initializers` | the default initializers of the members. |
| `parent` | reserved for an extended union; `nil` today. |

### Return values

None.

## Description

A **Union** lays every member out at the same address, so its size is the size
of its largest member. Fortran has no union type of its own; this node exists
to describe a C `union` reached through `bind(c)`, and to give LPython's
`Union` a representation.

Reading a member that was not the one last written is not diagnosed by ASR.

## Examples

An ASR text document that uses it:

```{literalinclude} ../../examples/union.asr
:language: clojure
```

## See Also

[UnionType](../type_nodes/UnionType.md), [UnionConstructor](../expression_nodes/UnionConstructor.md), [UnionInstanceMember](../expression_nodes/UnionInstanceMember.md), [Struct](Struct.md)
