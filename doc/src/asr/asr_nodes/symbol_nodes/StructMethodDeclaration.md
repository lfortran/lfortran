# StructMethodDeclaration

StructMethodDeclaration is a **symbol** node representing a type-bound procedure
declaration inside a derived type's `contains` section.

## Declaration

### Syntax

```
StructMethodDeclaration(symbol_table parent_symtab, identifier name,
    identifier? self_argument, identifier proc_name, symbol proc,
    abi abi, bool is_deferred, bool is_nopass)
```

### Arguments

`parent_symtab` the symbol table of the parent struct (derived type)

`name` the binding name (the name used after `%` in a method call)

`self_argument` identifies which dummy argument of the bound procedure receives
the passed object:
- `nullptr` — the passed object goes to the first dummy argument (default
  when `pass` is used without a name, or when neither `pass` nor `nopass` is
  specified)
- a name (e.g. `"pt"`) — the passed object goes to the dummy argument with
  that name, which may be at any position in the argument list (from
  `pass(pt)`)

`proc_name` the name of the actual procedure being bound

`proc` the symbol of the bound procedure (a `Function`)

`abi` abi such as: `Source`, `Interface`, `BindC`

`is_deferred` if true, this is a `deferred` binding (the procedure has no
implementation in this type and must be overridden in extending types)

`is_nopass` if true, no object is passed implicitly when the procedure is
called through this binding. When false, the object is passed as the argument
identified by `self_argument`

### Return values

None.

## Description

A `StructMethodDeclaration` represents a type-bound procedure — a procedure
declared in a derived type's `contains` section. It binds a name (the binding
name) to an actual procedure, with optional pass/nopass semantics controlling
how the invoking object is passed.

### Pass/NoPass Semantics

In Fortran, type-bound procedures can control whether and how the invoking
object is passed as an argument:

```fortran
type :: point
    real :: x, y
contains
    ! Default: pass on first argument (self_argument = null, is_nopass = false)
    procedure :: move => point_move

    ! Explicit pass on first argument (same as default)
    procedure, pass :: translate => point_translate

    ! Pass on a named argument (self_argument = "pt", is_nopass = false)
    procedure, pass(pt) :: scale => point_scale

    ! No pass: no implicit object argument (is_nopass = true)
    procedure, nopass :: create => point_create
end type
```

When `is_nopass` is false, calling `obj%method(args)` implicitly inserts `obj`
as an argument at the position determined by `self_argument`:
- If `self_argument` is null: `obj` becomes the first argument
- If `self_argument` is `"pt"`: `obj` is inserted at the position of the dummy
  argument named `"pt"` in the procedure's argument list

When `is_nopass` is true, calling `obj%method(args)` passes `args` directly
without inserting the object.

### Relationship to Variable

Procedure pointer *components* (declared in the data section of a derived type,
not in `contains`) are represented as [Variable](Variable.md) nodes with
`pass_attr` and `self_argument` fields that serve the same purpose as
`is_nopass` and `self_argument` in `StructMethodDeclaration`.

```fortran
type :: calculator
contains
    ! This is a StructMethodDeclaration
    procedure :: compute => compute_impl
    ! This is a Variable with type FunctionType (procedure pointer component)
    procedure(iface), nopass, pointer :: helper => null()
end type
```

## Examples

```fortran
module shapes
    type :: circle
        real :: radius
    contains
        procedure :: area => circle_area
    end type
contains
    function circle_area(self) result(a)
        class(circle), intent(in) :: self
        real :: a
        a = 3.14159 * self%radius**2
    end function
end module
```

In the ASR for type `circle`, the symbol table contains a
`StructMethodDeclaration`:

```
area:
    (StructMethodDeclaration
        area        -- binding name
        ()          -- self_argument (null = first arg)
        circle_area -- proc_name
        circle_area -- proc (symbol)
        Source       -- abi
        .false.      -- is_deferred
        .false.      -- is_nopass
    )
```

## See Also

[Variable](Variable.md)
