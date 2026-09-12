# Assignment

Assigns the value of an expression to a variable.

## Declaration

### Syntax

```text
Assignment(expr target, expr value, stmt? overloaded, bool realloc_lhs,
    bool move_allocation)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | what is assigned to: a variable, an array element or section, or a component. |
| `value` | the expression whose value is assigned. Its type must match the type of `target` exactly; any conversion is an explicit [Cast](../expression_nodes/Cast.md) inserted by the frontend. |
| `overloaded` | for a defined assignment, the [SubroutineCall](SubroutineCall.md) that implements it. A backend that does not care about the original spelling can lower this member and ignore the rest. |
| `realloc_lhs` | `true` when an allocatable `target` must be reallocated to the shape of `value` first. |
| `move_allocation` | `true` when the allocation of `value` is moved to `target` instead of being copied, as for `move_alloc`. |

### Return values

None.

## Description

Assignment in ASR never converts. `x = 1` where `x` is `real(8)` is an
**Assignment** whose `value` is a
[Cast](../expression_nodes/Cast.md) from `integer(4)` to `real(8)`: the
frontend decides the conversion, and the backend only lowers what it is given.

An array assignment is a whole-array operation, not a loop: the `array_op`
pass turns it into loops later.

## Examples

```clojure
(Assignment
  :target (Var
    :v (SymbolRef 1 "x")
  )
  :value (IntegerBinOp
    :left (IntegerConstant
      :n 2
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :op :Add
    :right (IntegerConstant
      :n 3
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
    :type (Integer
      :kind 4
    )
    :value (IntegerConstant
      :n 5
      :type (Integer
        :kind 4
      )
      :intboz_type :Decimal
    )
  )
  :overloaded nil
  :realloc_lhs false
  :move_allocation false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/program.asr
:language: clojure
```

## See Also

[Associate](Associate.md), [Cast](../expression_nodes/Cast.md), [SubroutineCall](SubroutineCall.md), [ReAlloc](ReAlloc.md)
