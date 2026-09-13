# Variable

A variable, a dummy argument, a named constant or a result variable.

## Declaration

### Syntax

```text
Variable(symbol_table parent_symtab, identifier name,
    identifier* dependencies, intent intent, expr? symbolic_value,
    expr? value, storage_type storage, ttype type,
    symbol? type_declaration, abi abi, access access,
    presence presence, bool value_attr, bool target_attr,
    bool contiguous_attr, string? bindc_name, bool is_volatile,
    bool is_protected, pass_attr pass_attr, identifier? self_argument,
    codimension* codims)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table this variable is stored in. |
| `name` | the name of the variable. |
| `dependencies` | the names of the symbols its type or initializer refers to. |
| `intent` | `Local` for a local variable, `In`, `Out`, `InOut` or `Unspecified` for a dummy argument, `ReturnVar` for the result variable of a function. |
| `symbolic_value` | the initializer as written, before folding. |
| `value` | the folded compile time value, when there is one. A `Parameter` always has one. |
| `storage` | `Default`, `Save` for a variable that keeps its value between calls, or `Parameter` for a named constant. |
| `type` | the type of the variable. |
| `type_declaration` | for a variable of a derived type, an enumeration or a union, the symbol that defines it; `nil` otherwise. |
| `abi` | `Source` when this ASR allocates the variable, otherwise the ABI of the definition it is shared with. |
| `access` | `Public` or `Private`. |
| `presence` | `Required`, or `Optional` for an optional dummy argument. |
| `value_attr` | `true` when a `bind(c)` dummy argument is passed by value. |
| `target_attr` | `true` for the `target` attribute, so a pointer may be associated with this variable. |
| `contiguous_attr` | `true` for the `contiguous` attribute. |
| `bindc_name` | the linker name given by `bind(c, name=...)`. |
| `is_volatile` | `true` for the `volatile` attribute: the value may change outside this code, so it must not be cached. |
| `is_protected` | `true` for the `protected` attribute: the variable is readable but not writable outside its module. |
| `pass_attr` | for a component holding a procedure pointer, whether the object is passed as an argument; `NotMethod` otherwise. |
| `self_argument` | the name of the passed-object dummy argument, when `pass_attr` is `Pass`. |
| `codims` | the codimensions of a coarray; empty for anything else. |

### Return values

None.

## Description

Everything a Fortran declaration says about a variable is stored here, so that
a backend never has to look at anything but the **Variable** and its `type` to
allocate it.

A variable is referenced from an expression by [Var](../expression_nodes/Var.md),
which holds nothing but a reference to this symbol; every property is read from
the symbol itself.

`symbolic_value` and `value` are different things. `symbolic_value` is the
initializer as the user wrote it, and `value` is what it folds to.
`storage=Parameter` requires `value`, since a named constant is substituted
wherever it is used.

## Examples

```clojure
(Variable
  :parent_symtab 1
  :name "n"
  :dependencies []
  :intent :Local
  :symbolic_value (IntegerConstant
    :n 10
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :value (IntegerConstant
    :n 10
    :type (Integer
      :kind 4
    )
    :intboz_type :Decimal
  )
  :storage :Parameter
  :type (Integer
    :kind 4
  )
  :type_declaration nil
  :abi :Source
  :access :Public
  :presence :Required
  :value_attr false
  :target_attr false
  :contiguous_attr false
  :bindc_name nil
  :is_volatile false
  :is_protected false
  :pass_attr :NotMethod
  :self_argument nil
  :codims []
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/variable.asr
:language: clojure
```

## See Also

[Var](../expression_nodes/Var.md), [Program](Program.md), [Function](Function.md), ttype
