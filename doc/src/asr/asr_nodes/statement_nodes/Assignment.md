# Assignment

Assigns the value of an expression to a variable.

## Declaration

### Syntax

```text
Assignment(expr target, expr value, stmt? overloaded, bool realloc_lhs,
    bool move_allocation, bool new_target, bool new_allocation)
```

### Arguments

| Argument | Description |
|----------|-------------|
| `target` | what is assigned to: a variable, an array element or section, or a component. |
| `value` | the expression whose value is assigned. Its type must match the type of `target` exactly; any conversion is an explicit [Cast](../expression_nodes/Cast.md) inserted by the frontend. |
| `overloaded` | for a defined assignment, the [SubroutineCall](SubroutineCall.md) that implements it. A backend that does not care about the original spelling can lower this member and ignore the rest. |
| `realloc_lhs` | `true` when an allocatable `target` must be reallocated to the shape of `value` first. |
| `move_allocation` | `true` when the allocation of `value` is moved to `target` instead of being copied, as for `move_alloc`. Both are allocatable arrays, or both are allocatable noncharacter scalars and `target` is type compatible with `value`; the object is neither copied nor finalized. |
| `new_target` | `true` when `target` is a new object that the assignment only defines, such as an element of an array allocated with `SOURCE=`. The value is copied into it: `target` is not finalized first, and its components are not assigned by defined assignment, as they are for the variable of an intrinsic assignment. Such an assignment is neither a move nor a defined assignment. |
| `new_allocation` | `true` when `target` is an allocatable variable that was unallocated and has just been allocated for this intrinsic assignment, by the [Allocate](Allocate.md) before it. Being an unallocated allocatable variable, `target` is not finalized first, but it is otherwise assigned as the variable of an intrinsic assignment: its components are assigned by defined assignment where they have one. Such an assignment is neither a move, a defined assignment, nor the definition of a new target. |

### Return values

None.

## Description

Assignment in ASR never converts. `x = 1` where `x` is `real(8)` is an
**Assignment** whose `value` is a
[Cast](../expression_nodes/Cast.md) from `integer(4)` to `real(8)`: the
frontend decides the conversion, and the backend only lowers what it is given.

An array assignment is a whole-array operation, not a loop: the `array_op`
pass turns it into loops later.

An assignment to an allocatable scalar of derived type allocates it when it
is unallocated (F2018 10.2.1.3). The `allocate_on_assignment` pass makes this
explicit: it inserts an [Allocate](Allocate.md) of `target` before the
assignment, guarded by `allocated(target)`. When the type is finalizable, the assignment is
duplicated into both branches of an [If](If.md): the one to the target the
pass allocated has `new_allocation`, as that target is not finalized
(F2018 7.5.6.3), and the other one does not. A polymorphic allocatable scalar
assigned a value that is not polymorphic takes the type of the value: the
pass deallocates it with an [ImplicitDeallocate](ImplicitDeallocate.md),
which finalizes it when it is allocated, allocates it with that type, and
gives the assignment `new_allocation`. A polymorphic value gives it its
dynamic type: an allocated variable of another dynamic type is deallocated,
and an unallocated one is allocated with `MOLD=` the value and assigned with
`new_allocation`, while one of the same dynamic type, as for `a = a`, is
assigned as it is. A backend then only lowers the allocation and the
assignment it is given.

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
  :new_target false
  :new_allocation false
)
```

It comes from this complete ASR text document:

```{literalinclude} ../../examples/program.asr
:language: clojure
```

## See Also

[Associate](Associate.md), [Cast](../expression_nodes/Cast.md), [SubroutineCall](SubroutineCall.md), [ReAlloc](ReAlloc.md)
