# AssociateBlock

The scope of an `associate` construct.

## Declaration

### Syntax

```text
AssociateBlock(symbol_table symtab, identifier name, stmt* body)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `symtab` | the symbol table of the construct, holding the associate names. |
| `name` | a generated name for the block, unique in its scope. |
| `body` | the statements of the construct. |

### Return values

None.

## Description

An `associate` construct introduces names for expressions and runs a block of
statements with them in scope, so it needs a symbol table of its own. That
makes it a symbol rather than a statement: the symbol holds the scope, and
[AssociateBlockCall](../statement_nodes/AssociateBlockCall.md) marks the place
in the enclosing statement list where the block runs.

Each associate name is set up by an
[Associate](../statement_nodes/Associate.md) statement at the start of the
body, which binds the name to the target expression rather than copying it.

## Examples

```clojure
(AssociateBlock
  :symtab (SymbolTable
    :id 2
    :symbols {
      "k" (Variable
        :parent_symtab 2
        :name "k"
        :dependencies []
        :intent :Local
        :symbolic_value nil
        :value nil
        :storage :Default
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
    }
  )
  :name "block"
  :body [
    (Associate
      :target (Var
        :v (SymbolRef 2 "k")
      )
      :value (Var
        :v (SymbolRef 1 "x")
      )
    )
    (Assignment
      :target (Var
        :v (SymbolRef 2 "k")
      )
      :value (IntegerConstant
        :n 1
        :type (Integer
          :kind 4
        )
        :intboz_type :Decimal
      )
      :overloaded nil
      :realloc_lhs false
      :move_allocation false
    )
  ]
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/associateblock.asr
:language: clojure
```

## See Also

[AssociateBlockCall](../statement_nodes/AssociateBlockCall.md), [Associate](../statement_nodes/Associate.md), [Block](Block.md)
