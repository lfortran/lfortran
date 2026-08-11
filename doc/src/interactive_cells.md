# Interactive evaluation: one TranslationUnit per cell

This page describes how LFortran represents an interactive session — the REPL,
the Jupyter kernel and the JupyterLite lab — in ASR, and what that means for
code that walks symbol tables. Ordinary (ahead-of-time) compilation is not
affected by any of it: there is one `TranslationUnit`, no chaining and no
copying.

## The shape

Every cell is compiled into its own `TranslationUnit`. Their symbol tables are
chained through `SymbolTable::parent`, oldest first:

```
cell 1   TranslationUnit   symtab(parent = nullptr)
cell 2   TranslationUnit   symtab(parent = cell 1's symtab)
cell 3   TranslationUnit   symtab(parent = cell 2's symtab)
```

Two properties follow directly from that shape:

* **Cells are additive.** A cell sees everything declared before it, and
  nothing can reach forward into a later cell.
* **Redeclaring shadows.** A name declared again in a later cell does not
  overwrite the earlier one; it hides it, the way a nested scope does. Code
  compiled earlier keeps using what it resolved to then, and later cells
  resolve to the new declaration. This is what a Python notebook does when you
  re-run a cell: the name is rebound, and objects created earlier keep the
  binding they were made with.

So this session prints `1`, then `2`, then `1` again:

```fortran
integer function f()
f = 1
end function
```
```fortran
f()          ! 1
```
```fortran
integer function f()
f = 2
end function
```
```fortran
f()          ! 2
```

The first `f` is still there and still returns 1 — Fortran simply gives you no
way to name it once it is shadowed. The same holds for variables and modules;
a module redefined in a later cell leaves procedures compiled against the
earlier one using the earlier one.

## Walking the chain

Before cells were chained, a scope chain ended at the one and only
`TranslationUnit`, so "walk to the root" and "find the translation unit" were
the same thing. They are no longer. A walk looking for the translation unit has
to stop at the *nearest* one, which is the cell being compiled — otherwise
symbols generated during compilation land in the session's first cell, which
was compiled long ago and cannot be added to any more.

`ASRUtils::is_tu_scope()` is the test, and the walks that use it are
`get_tu_symtab()`, `get_sym_module()`, `get_sym_module0()`,
`get_scope_names()` and `SymbolTable::get_global_scope()`. With a single
`TranslationUnit` the nearest one is also the root, so ordinary compilation
sees no change. **New code that walks up a scope chain looking for the
translation unit should stop at `is_tu_scope()`, not at `parent == nullptr`.**

## Qualifying symbols by cell

Two live declarations of the same name need two distinct symbols in the
generated code. Symbols declared at cell scope are therefore qualified by the
cell they belong to, the way symbols in a module are qualified by the module:
`ASRUtils::cell_prefix()` returns `__cell<N>_` for the Nth cell.

The first cell is deliberately unqualified, so that it and ordinary compilation
produce identical symbols. The wrapper each cell is run through
(`__lfortran_evaluate_<N>`) is never qualified — the evaluator looks it up by
that name.

Codegen derives the prefix from the scope that *declares* a symbol, not from
the cell being compiled (`tu_symbol_prefix()` in `asr_to_llvm.cpp`), which is
what makes a reference from a later cell resolve to the earlier cell's symbol.

## The snapshot

The ASR passes rewrite what they are given: `pass_array_by_data` replaces a
procedure taking an assumed-shape array with a specialisation under a mangled
name, for instance. A cell is compiled from the tree semantic analysis
produced, and the passes may do as they like to it — it is thrown away
afterwards. What the *next* cell is parented to is a copy of this cell's
symbols taken beforehand (`FortranEvaluator::snapshot_cell_scope()`), so that
later cells resolve names against the signatures the user wrote rather than
the lowered ones.

Only symbols are copied. Later cells resolve names and read signatures; they
never re-run an earlier cell's statements.

This has a consequence worth knowing: because a later cell resolves to the
*unspecialised* procedure, a pass that changes signatures has to specialise
earlier cells' procedures too, so that the call reaches the procedure the JIT
actually holds. The generated names are derived from the signature, so they
come out the same as they did when that cell was compiled, and the call links
to the definition already in the JIT. `pass_array_by_data` does this by
visiting ancestor cell scopes.

## Codegen and the JIT

When a cell is compiled, earlier cells' symbols are *declared*, not defined:
their definitions are already in the JIT, and this cell holds them in their
pre-pass form anyway. Compiler-generated helpers are recreated by the passes on
every evaluation, and redefining a symbol is an error for the JIT, so
`FortranEvaluator::drop_redefinitions()` turns definitions the JIT already
holds into declarations.

A cell containing a `program` unit is compiled into
`__lfortran_evaluate_<N>_program` and called from the evaluator; without that,
the program would be compiled and never run, and the cell would silently
produce no output.

## Tests

`src/lfortran/tests/test_llvm.cpp` holds the interactive tests. The shadowing
ones (`FortranEvaluator shadow a variable/function/module across cells`) are
the executable description of the semantics above: each checks that the new
binding is visible *and* that the old one is still alive.

One cell is one `evaluate2()` call, so a test is written as a sequence of them.
See {doc}`jupyterlite` for reproducing a lab bug in the browser and reducing it
to such a test.
