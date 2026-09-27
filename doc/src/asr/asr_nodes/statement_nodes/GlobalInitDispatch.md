# GlobalInitDispatch

Enters the startup engine at an entry point of the program.

## Declaration

### Syntax

```text
GlobalInitDispatch(init_dispatch_phase phase, stmt* ensures)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `phase` | which phase of the engine to run: [InitDispatchLocal or InitDispatchCollective](../enum_nodes/init_dispatch_phase.md). |
| `ensures` | calls of the startup initializers this entry point needs itself, each an ordinary `SubroutineCall` of an initializer an owner's `global_init` names. They run after the dispatch. |

### Return values

None.

## Description

**GlobalInitDispatch** is where generated code enters the one startup engine
that runs every startup initializer of the program before anything can
observe the state it initializes; see the *Startup initializers* section of
[Program](../symbol_nodes/Program.md). It appears only as a top-level
statement of the body of a [Program](../symbol_nodes/Program.md) or a
[Function](../symbol_nodes/Function.md), at most once, and `asr_verify`
rejects it anywhere else.

The `global_init_wire` pass makes it the first statement of every program,
with the collective phase and no `ensures`: a main program is the collective
boundary at which every image runs the same startup before its first
statement. The `global_init` pass puts one with the local phase and the
initializers of the owners the procedure uses into every `bind(c)` procedure
written by the user, a module procedure, an external procedure or an
internal procedure of a program, which is where a foreign caller enters. The `ensures`
are what guarantees those initializers to that caller even when the engine
is already dispatching on the calling thread, where entering it again
returns at once.

A backend emits it at the actual entry of the procedure, before the
arguments are converted and before any specification expression or automatic
object is evaluated, whichever top-level position the passes left it in. A
backend whose output is linked with other code lowers it to a call of the
runtime engine followed by the `ensures`: `_lcompilers_init_dispatch` for the
collective phase, and for the local phase `_lcompilers_init_enter`, passed a
zero-initialized word of the procedure's own, through which a call returns
at once while nothing that registers records has changed since a dispatch
through it completed. A
backend whose output is the whole program lowers it through
`ASRUtils::expand_closed_world_dispatch` instead, into calls of every
initializer the program has.

## Examples

```clojure
(GlobalInitDispatch
  :phase :InitDispatchLocal
  :ensures [
    (SubroutineCall
      :name (SymbolRef 3 "__lcompilers_global_init_m")
      :original_name (SymbolRef 3 "__lcompilers_global_init_m")
      :args []
      :dt nil
      :strict_bounds_checking false
    )
  ]
)
```

It comes from this complete ASR text document, the output of the
`global_init` and `global_init_wire` passes, where the program starts with
the collective dispatch as well:

```{literalinclude} ../../examples/global_init.asr
:language: clojure
```

A `bind(c)` procedure one of whose variables has bounds or a length that
read startup state is split, so that the dispatch runs before they are
evaluated: the procedure stays the entry, with the dispatch and a call of an
implementation that holds the user's declarations and body. The
implementation is a sibling of the entry in the same host, here module `m`,
and what the dummies are declared with, such as the local type `pair`, moves
to that host, where both see it. The entry's body is then

```clojure
(GlobalInitDispatch
  :phase :InitDispatchLocal
  :ensures [
    (SubroutineCall
      :name (SymbolRef 3 "__lcompilers_global_init_m")
      :original_name (SymbolRef 3 "__lcompilers_global_init_m")
      :args []
      :dt nil
      :strict_bounds_checking false
    )
  ]
)
(Assignment
  :target (Var
    :v (SymbolRef 7 "p")
  )
  :value (FunctionCall
    :name (SymbolRef 3 "__lcompilers_impl_m_p")
    :original_name (SymbolRef 3 "__lcompilers_impl_m_p")
    :args [
      (call_arg
        :value (Var
          :v (SymbolRef 7 "x")
        )
      )
    ]
    :type (Integer
      :kind 4
    )
    :value nil
    :dt nil
  )
  :overloaded nil
  :realloc_lhs false
  :move_allocation false
)
```

from this complete ASR text document:

```{literalinclude} ../../examples/global_init_entry.asr
:language: clojure
```

## See Also

[GlobalInitStorage](GlobalInitStorage.md), [init_dispatch_phase](../enum_nodes/init_dispatch_phase.md), [Program](../symbol_nodes/Program.md), [Module](../symbol_nodes/Module.md)
