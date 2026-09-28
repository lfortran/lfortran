# ModuleReference

A name that designates a module.

## Declaration

### Syntax

```text
ModuleReference(symbol_table parent_symtab, identifier name,
    identifier module_name, access access)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `parent_symtab` | the symbol table this name is stored in. |
| `name` | the local name, `L` in `use, namespace :: L => M`. |
| `module_name` | the name the module is registered under in the translation unit, the name written in the USE statement. |
| `access` | whether a module that declares this name exports it. |

### Return values

None.

## Description

A module reference is declared by a namespace import,
`use, namespace :: L => M` (an LFortran extension, see
[Namespaces for Modules](../../../namespace_modules.md)). It makes only the
name `L` accessible, and the entities of the module are reached as `L%x`.

The frontend resolves every `L%x` to an [ExternalSymbol](ExternalSymbol.md)
for `x` in the scope of the reference, under a name that cannot clash with an
identifier, `M%x`, with `Private` access. Expressions, statements, passes and
backends therefore never see a module reference; they see ordinary external
symbols.

A module reference is a symbol so that it follows the rules of every other
name: it is host associated into nested scopes, and a public module reference
of a module is use associated by users of that module (an
[ExternalSymbol](ExternalSymbol.md) whose `external` is the module reference)
and saved in its module file. That is what makes `b%a%x` work when module `b`
imports module `a` as a namespace.

## Examples

```clojure
(ModuleReference
  :parent_symtab 2
  :name "n"
  :module_name "m"
  :access :Public
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/modulereference.asr
:language: clojure
```

## See Also

[ExternalSymbol](ExternalSymbol.md), [Module](Module.md)
