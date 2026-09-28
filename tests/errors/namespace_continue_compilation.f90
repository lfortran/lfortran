! Syntax and semantic errors of namespace imports (`use, namespace`, see
! doc/src/namespace_modules.md), all reported with --continue-compilation.
! Each case is a separate program unit, headed by a comment saying what it
! checks. The syntax errors (cc_59 to cc_63) are reported first, since the
! whole file is parsed before semantics; the parser skips the offending
! statement and continues. The cases cc_64 and cc_65 use modules compiled
! separately (the extrafiles namespace_separate_types.f90 and
! namespace_separate_holder.f90), loaded from their modfiles.

module nscc_m
    implicit none
    private :: hidden
    integer :: x = 1
    integer, protected :: p = 1
    integer, parameter :: n = 3
    integer :: visible = 1
    integer :: hidden = 2
    type :: t
        integer :: x = 1
    end type
end module

module nscc_m2
    implicit none
    integer :: x = 2
    type :: t
        integer :: i = 2
    end type
end module

! Module entity vs use-associated variable of the same name (cc_11)
module nscc_u
    implicit none
    type :: ut
        integer :: x = 2
    end type
    type(ut) :: u
end module

! Defined operator (cc_18)
module nscc_op
    implicit none
    type :: vec_t
        real :: x = 0
    end type
    interface operator(.dot.)
        module procedure dot
    end interface
contains
    real function dot(a, b)
        type(vec_t), intent(in) :: a, b
        dot = a%x*b%x
    end function
end module

! Module entities of other modules (cc_28, cc_29, cc_33, cc_35)
module nscc_private_a
    use, namespace :: a => nscc_m
    implicit none
    private :: a
end module

module nscc_export_a1
    use, namespace :: a => nscc_m
    implicit none
    public :: a
end module

module nscc_export_a2
    use, namespace :: a => nscc_m2
    implicit none
    public :: a
end module

module nscc_private_decl
    use, namespace, private :: a => nscc_m
    implicit none
    integer :: y = 2
end module

module nscc_export_m
    use, namespace :: nscc_m
    implicit none
end module

module nscc_export_m_as_m2
    use, namespace :: nscc_m => nscc_m2
    implicit none
end module

! Generic interface (cc_30)
module nscc_gen
    implicit none
    interface swap
        module procedure swap_int
    end interface
contains
    subroutine swap_int(a, b)
        integer, intent(inout) :: a, b
        integer :: t
        t = a; a = b; b = t
    end subroutine
end module

! A local generic interface does not extend a generic accessed through a
! module entity, so g%swap has no specific for character arguments.
module cc_30
    use, namespace :: g => nscc_gen
    implicit none
    interface swap
        module procedure swap_char
    end interface
contains
    subroutine swap_char(a, b)
        character(len=*), intent(inout) :: a, b
        character(len=len(a)) :: t
        t = a; a = b; b = t
    end subroutine
    subroutine s(c, d)
        character(len=*), intent(inout) :: c, d
        call g%swap(c, d)
    end subroutine
end module

! At most one access-spec may appear in a namespace import.
module cc_32
    use, namespace, public, private :: a => nscc_m
    implicit none
end module

! A module cannot import itself as a module entity.
module cc_22
    use, namespace :: self => cc_22
    implicit none
    integer :: x = 1
end module

! A namespace import does not make the members accessible without
! qualification.
subroutine cc_01()
    use, namespace :: nscc_m
    implicit none
    print *, x
end subroutine

! ONLY cannot be combined with a namespace import.
subroutine cc_02()
    use, namespace :: nscc_m, only: x
    implicit none
end subroutine

! The module has no entity with that name.
subroutine cc_04()
    use, namespace :: m => nscc_m
    implicit none
    print *, m%nosuch
end subroutine

! Private module entities are not accessible through a module entity.
subroutine cc_05()
    use, namespace :: m => nscc_m
    implicit none
    print *, m%visible, m%hidden
end subroutine

! A module entity is not a data object; it cannot appear in an expression by
! itself.
subroutine cc_06()
    use, namespace :: m => nscc_m
    implicit none
    print *, m
end subroutine

! A module entity cannot be passed as an actual argument.
subroutine cc_07()
    use, namespace :: m => nscc_m
    implicit none
    call show(m)
contains
    subroutine show(a)
        integer, intent(in) :: a
        print *, a
    end subroutine
end subroutine

! A module entity cannot be assigned to.
subroutine cc_08()
    use, namespace :: m => nscc_m
    implicit none
    m = 1
end subroutine

! The module entity's name clashes with a local entity of the same scope.
subroutine cc_09()
    use, namespace :: m => nscc_m
    implicit none
    integer :: m
end subroutine

! Two namespace imports give the same local name to different modules.
subroutine cc_10()
    use, namespace :: u => nscc_m
    use, namespace :: u => nscc_m2
    implicit none
end subroutine

! A module entity and a use-associated entity have the same local name;
! referencing the name is ambiguous.
subroutine cc_11()
    use, namespace :: u => nscc_m
    use nscc_u, only: u
    implicit none
    print *, u%x
end subroutine

! A PROTECTED variable cannot be modified outside its module, also when
! accessed through a module entity.
subroutine cc_12()
    use, namespace :: m => nscc_m
    implicit none
    print *, m%p
    m%p = 2
end subroutine

! An ordinary USE statement does not create a module entity; the module name
! cannot be used as a qualifier.
subroutine cc_13()
    use nscc_m
    implicit none
    print *, x
    print *, nscc_m%x
end subroutine

! A module that is not used at all cannot be used as a qualifier.
subroutine cc_14()
    implicit none
    print *, nscc_m2%x
end subroutine

! The NAMESPACE modifier appears twice.
subroutine cc_15()
    use, namespace, namespace :: m => nscc_m
    implicit none
end subroutine

! A module can only be renamed in a namespace import; an ordinary USE
! statement cannot rename the module.
subroutine cc_17()
    use :: m => nscc_m
    implicit none
end subroutine

! Non-type-bound defined operators are not imported by a namespace import
! (they have no name that could be qualified).
subroutine cc_18()
    use, namespace :: v => nscc_op
    implicit none
    type(v%vec_t) :: a, b
    print *, a .dot. b
end subroutine

! In an internal procedure, a local variable hides the host's module entity
! of the same name, so "m%x" refers to a component of an integer.
subroutine cc_21()
    use, namespace :: m => nscc_m
    implicit none
    call sub()
contains
    subroutine sub()
        integer :: m
        m = 2
        print *, m%x
    end subroutine
end subroutine

! A derived type accessed through a module entity is not a data object.
subroutine cc_24()
    use, namespace :: m => nscc_m
    implicit none
    print *, m%t%x
end subroutine

! A named constant accessed through a module entity cannot be assigned to.
subroutine cc_27()
    use, namespace :: m => nscc_m
    implicit none
    m%n = 4
end subroutine

! A module entity that a module declares PRIVATE is not accessible to users
! of that module through a module entity of the module.
subroutine cc_28()
    use, namespace :: b => nscc_private_a
    implicit none
    print *, b%a%x
end subroutine

! Two modules export module entities with the same local name for different
! modules; referencing that name is ambiguous.
subroutine cc_29()
    use nscc_export_a1
    use nscc_export_a2
    implicit none
    print *, a%x
end subroutine

! An access-spec in a namespace import is only allowed in the specification
! part of a module, like an accessibility statement.
subroutine cc_31()
    use, namespace, private :: m => nscc_m
    implicit none
end subroutine

! A module entity declared "use, namespace, private" is not accessible to
! users of the module, neither by ONLY nor by a plain USE.
subroutine cc_33()
    use nscc_private_decl, only: a
    implicit none
end subroutine

! A module name is not a module entity (D2), also when a module entity for
! the same module was used before in the scope.
subroutine cc_34()
    use, namespace :: m => nscc_m
    implicit none
    type(m%t) :: a
    type(nscc_m%t) :: b
end subroutine

! An ambiguous module entity (two use-associated module entities with the
! same local name for different modules) in a type-spec, where its local name
! is the name of one of the modules and the host already used that module's
! type through another module entity.
subroutine cc_35()
    use, namespace :: m => nscc_m
    implicit none
    type(m%t) :: a
    a%x = 2
contains
    subroutine sub()
        use nscc_export_m
        use nscc_export_m_as_m2
        type(nscc_m%t) :: b
    end subroutine
end subroutine

! A module entity is not a type or an interface.
subroutine cc_36()
    use, namespace :: m => nscc_m
    implicit none
    type(m) :: a
end subroutine

! A variable accessed through a module entity is not a type.
subroutine cc_37()
    use, namespace :: m => nscc_m
    implicit none
    type(m%x) :: a
end subroutine

! A module entity is not a type (CLASS).
subroutine cc_38()
    use, namespace :: m => nscc_m
    implicit none
    class(m), allocatable :: a
end subroutine

! A module entity is not an interface.
subroutine cc_39()
    use, namespace :: m => nscc_m
    implicit none
    procedure(m), pointer :: p
end subroutine

! A variable accessed through a module entity is not a subroutine.
subroutine cc_40()
    use, namespace :: m => nscc_m
    implicit none
    call m%x()
end subroutine

! A module entity is not a variable, so it cannot have the SAVE attribute.
module cc_41
    use, namespace :: m => nscc_m
    implicit none
    save :: m
end module

! A module entity cannot be a named constant.
module cc_42
    use, namespace :: m => nscc_m
    implicit none
    parameter (m = 3)
end module

! Generic function (cc_43, cc_45)
module nscc_fgen
    implicit none
    interface gen
        module procedure gen_int
    end interface
contains
    pure integer function gen_int(i)
        integer, intent(in) :: i
        gen_int = i
    end function
end module

! A generic function accessed through a module entity in an array bound of a
! module variable: the bound is not a constant expression.
module cc_43
    use, namespace :: g => nscc_fgen
    implicit none
    integer :: w(g%gen(2))
end module

! A module entity is not a type to extend.
module cc_44
    use, namespace :: m => nscc_m
    implicit none
    type, extends(m) :: u
    end type
end module

! A function accessed through a module entity is not a type to extend.
module cc_45
    use, namespace :: g => nscc_fgen
    implicit none
    type, extends(g%gen_int) :: u
    end type
end module

! Derived types and a procedure with a dummy argument of one of them
! (cc_46 to cc_48)
module nscc_types
    implicit none
    type :: t
        integer :: k = 1
    end type
    type :: u
        integer :: k = 2
    end type
contains
    subroutine take(v)
        type(t), intent(in) :: v
    end subroutine
end module

! A structure constructor accessed through a module entity passed for a dummy
! argument of another type: the type is shown by its name, `u`.
subroutine cc_46()
    use nscc_types, only: take
    use, namespace :: l => nscc_types
    implicit none
    call take(l%u(1))
end subroutine

! An array constructor with a type-spec accessed through a module entity
! passed for a scalar dummy argument of another type.
subroutine cc_47()
    use nscc_types, only: take
    use, namespace :: l => nscc_types
    implicit none
    call take([l%u :: l%u(3)])
end subroutine

! A structure constructor accessed through a module entity assigned to a
! variable of another type.
subroutine cc_48()
    use, namespace :: l => nscc_types
    implicit none
    type(l%t) :: y
    y = l%u(1)
end subroutine

! A module entity is not a type to allocate.
subroutine cc_49()
    use, namespace :: m => nscc_m
    implicit none
    class(m%t), allocatable :: y
    allocate(m :: y)
end subroutine

! A variable accessed through a module entity is not a type to allocate.
subroutine cc_50()
    use, namespace :: m => nscc_m
    implicit none
    class(m%t), allocatable :: y
    allocate(m%x :: y)
end subroutine

! A function accessed through a module entity is not a type to allocate.
subroutine cc_51()
    use, namespace :: m => nscc_m
    use, namespace :: g => nscc_fgen
    implicit none
    class(m%t), allocatable :: y
    allocate(g%gen_int :: y)
end subroutine

! Two sibling procedures import the same module under different local names:
! the type is shown by its name, not as written in the other procedure.
subroutine cc_52()
    implicit none
contains
    subroutine s1()
        use, namespace :: l => nscc_types
        implicit none
        type(l%u) :: w
        w = l%u(1)
    end subroutine
    subroutine s2()
        use nscc_types, only: take
        use, namespace :: k => nscc_types
        implicit none
        call take(k%u(1))
    end subroutine
end subroutine

! Another derived type named `u` and a procedure with a dummy argument of it
! (cc_53)
module nscc_types2
    implicit none
    type :: u
        integer :: k = 3
    end type
contains
    subroutine take_u(v)
        type(u), intent(in) :: v
    end subroutine
end module

! `l` designates another module than in the sibling procedure `s1`: the type
! is shown by its name, not as `l%u`, which would designate the expected type.
subroutine cc_53()
    implicit none
contains
    subroutine s1()
        use, namespace :: l => nscc_types
        implicit none
        type(l%u) :: w
        w = l%u(1)
    end subroutine
    subroutine s2()
        use, namespace :: l => nscc_types2
        use, namespace :: k => nscc_types
        implicit none
        call l%take_u(k%u(1))
    end subroutine
end subroutine

! A derived type with a component declared through a module entity (cc_54)
module nscc_holder
    use, namespace :: l => nscc_types
    implicit none
    type :: holder
        type(l%u) :: c
    end type
end module

! The component, declared as `type(l%u)` in another module, passed for a
! dummy argument of another type in a scope without `l`.
subroutine cc_54()
    use nscc_types, only: take
    use nscc_holder, only: holder
    implicit none
    type(holder) :: h
    call take(h%c)
end subroutine

! A structure constructor of a derived type renamed by an ordinary USE: the
! type is also shown by its name, `u`, not by the local name `uu`.
subroutine cc_55()
    use nscc_types, only: take, uu => u
    implicit none
    call take(uu(1))
end subroutine

! ALLOCATE of a component that is neither allocatable nor a pointer,
! declared as `type(l%u)` in another module: its type is shown as `u`.
subroutine cc_56()
    use nscc_holder, only: holder
    implicit none
    type(holder) :: h
    allocate(h%c)
end subroutine

! DEALLOCATE of a structure constructor accessed through a module entity:
! its type is shown as `u`.
subroutine cc_57()
    use, namespace :: l => nscc_types
    implicit none
    deallocate(l%u(1))
end subroutine

! A parameterized derived type with default type parameters (cc_58)
module nscc_pdt
    implicit none
    type :: pt(k)
        integer, kind :: k = 4
        integer(k) :: v = 1
    end type
end module

! A variable of a parameterized derived type declared as `type(l%pt)` passed
! for a dummy argument of another type: its type is shown as `pt_4`, the
! instance of `pt` for k = 4, as for `type(pt)`.
subroutine cc_58()
    use nscc_types, only: take
    use, namespace :: l => nscc_pdt
    implicit none
    type(l%pt) :: b
    call take(b)
end subroutine

! A rename list cannot be combined with a namespace import: syntax error.
subroutine cc_59()
    use, namespace :: m => nscc_m, y => x
    implicit none
end subroutine

! The double colon is required after the NAMESPACE modifier, as for the
! INTRINSIC and NON_INTRINSIC modifiers: syntax error.
subroutine cc_60()
    use, namespace m => nscc_m
    implicit none
end subroutine

! A DO variable must be a variable name, not a module-qualified name: syntax
! error. The loop is on one line so that its END DO is skipped with it.
subroutine cc_61()
    use, namespace :: m => nscc_m
    implicit none
    do m%x = 1, 3; end do
end subroutine

! A named constant for the kind parameter of a literal constant (cc_62)
module nscc_kinds
    implicit none
    integer, parameter :: dp = kind(1.0d0)
end module

! The kind parameter of a literal constant must be a digit string or a named
! constant name; a module-qualified name is not allowed: syntax error. Use
! real(1.5, m%dp) instead.
subroutine cc_62()
    use, namespace :: m => nscc_kinds
    implicit none
    real :: y
    y = 1.5_m%dp
    print *, y
end subroutine

! An entity cannot be declared inside a module entity; a qualified name is not
! an object name: syntax error.
subroutine cc_63()
    use, namespace :: m => nscc_m
    implicit none
    integer :: m%y
    print *, m%x
end subroutine

! The component, declared as `type(l%u)` in the separately compiled module
! nssep_holder, is passed for a dummy argument of another type: the type is
! shown by its name, `u`.
subroutine cc_64()
    use nssep_types, only: take
    use nssep_holder, only: holder
    implicit none
    type(holder) :: h
    call take(h%c)
end subroutine

! In a submodule of the separately compiled module nssep_parent, which declared
! the symbol for `l%u`, a structure constructor `l%u(1)` is passed for a dummy
! argument of another type: the type is shown by its name, `u`.
submodule (nssep_parent) cc_65
    implicit none
contains
    module subroutine run()
        call l%take(l%u(1))
    end subroutine
end submodule

program namespace_continue_compilation
    implicit none
end program
