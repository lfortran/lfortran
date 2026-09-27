# Module

A Fortran module or submodule.

## Declaration

### Syntax

```text
Module(symbol_table symtab, identifier name, identifier? parent_module,
    identifier* dependencies, bool loaded_from_mod, bool intrinsic,
    bool has_submodules, identifier? global_init,
    identifier? global_init_state, bool global_init_collective,
    location start_name, location end_name)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the module, owning everything the module declares. |
| `name` | the name of the module. |
| `parent_module` | for a submodule, the name of the module it extends; `nil` for a module. |
| `dependencies` | the names of the modules this module uses. |
| `loaded_from_mod` | `true` when the module was read back from a module file rather than compiled from source in this run. |
| `intrinsic` | `true` for a module defined by the language itself (`iso_c_binding`, `iso_fortran_env`, ...). The backends do not emit code for it. |
| `has_submodules` | `true` when at least one submodule extends this module. A module procedure declared here may then be defined elsewhere. |
| `global_init` | the name of this module's startup initializer in `symtab`: an argument-less procedure that initializes the module's variables, once, before any code can observe them. Every user module and submodule owns one from the moment semantics creates it, whatever its initialization later lowers to, so that its module file carries it and every translation unit that depends on the module calls that one definition. It is `nil` for an intrinsic module and for a module a pass creates without one, such as the one that holds a COMMON block, whose storage the translation unit's initializer sets up. See [Program](Program.md). |
| `global_init_state` | the name of the saved `integer(4)` variable in `symtab` that guards `global_init`: not initialized, being initialized, or ready. `nil` exactly when `global_init` is. |
| `global_init_collective` | `true` when the initializer needs a collective boundary: the module allocates a saved coarray anywhere in its scope, or it depends on a module that does. Computed by the `coarray` pass and carried in the module file. |
| `start_name` | the source span of the name in `module name`. |
| `end_name` | the source span of the name in `end module name`. |

### Return values

None.

## Description

A **Module** owns the symbols it declares. Code in another program unit never
looks a module symbol up through this table directly: it declares an
[ExternalSymbol](ExternalSymbol.md) in its own scope that points at the symbol
here.

When a module is compiled, the full ASR (`abi=Source`, non-empty procedure
bodies) and the interface ASR (`abi=LFortranModule`, empty bodies) are both
written to the module file. `loaded_from_mod` records which side of that a
particular **Module** came from.

## Examples

```clojure
(Module
  :symtab (SymbolTable
    :id 1
    :symbols {
      "pi" (Variable
        :parent_symtab 1
        :name "pi"
        :dependencies []
        :intent :Local
        :symbolic_value (RealConstant
          :r 3.1415926535897931
          :type (Real
            :kind 8
          )
        )
        :value (RealConstant
          :r 3.1415926535897931
          :type (Real
            :kind 8
          )
        )
        :storage :Parameter
        :type (Real
          :kind 8
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
    }
  )
  :name "constants"
  :parent_module nil
  :dependencies []
  :loaded_from_mod false
  :intrinsic false
  :has_submodules false
  :global_init nil
  :global_init_state nil
  :global_init_collective false
)
```

It comes from this complete ASR text document, which describes the module on
its own, without the initializer semantics gives every user module:

```{literalinclude} ../../examples/module.asr
:language: clojure
```

## See Also

[Program](Program.md), [Function](Function.md), [ExternalSymbol](ExternalSymbol.md), [Variable](Variable.md), [GlobalInitDispatch](../statement_nodes/GlobalInitDispatch.md), [GlobalInitStorage](../statement_nodes/GlobalInitStorage.md)
