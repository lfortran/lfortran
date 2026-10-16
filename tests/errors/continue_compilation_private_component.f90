! Structure constructors that give a value to a component that is not
! accessible where they appear (F2018 7.5.10, C7102). A private component is
! accessible only in the module that defines it and in that module's
! descendants (7.5.4.8), so outside it a constructor cannot give one by
! keyword or by position, through a renamed or re-exported type, in a
! component's default initialization or a named constant, or when the
! component is inherited by an extension defined elsewhere. The parent
! component of an extension of a private type is private too (7.5.7.2), and a
! generic interface named like the type that accepts no such arguments leaves
! the structure constructor, which is then checked.
!
! Each inaccessible component is reported, two of them for one constructor,
! and compilation continues with the next statement or unit.
module priv_ctor_mod
    implicit none
    private
    public :: priv_ctor_t, priv_ctor_mixed_t, priv_ctor_base_t, priv_ctor_child_t
    public :: priv_ctor_overloaded_t, priv_ctor_hidden_value, priv_ctor_hidden_var
    type :: priv_ctor_t
        private
        integer :: a
        integer :: b = 0
    end type
    type :: priv_ctor_mixed_t
        integer :: p
        integer, private :: q = 3
    end type
    type :: priv_ctor_base_t
        private
        integer :: x = 0
    end type
    type :: priv_ctor_hidden_t
        integer :: h = 0
    end type
    type, extends(priv_ctor_hidden_t) :: priv_ctor_child_t
        integer :: c = 0
    end type
    type(priv_ctor_hidden_t) :: priv_ctor_hidden_var
    type :: priv_ctor_overloaded_t
        private
        integer :: n
    end type
    interface priv_ctor_overloaded_t
        procedure :: priv_ctor_new
    end interface
contains
    function priv_ctor_hidden_value() result(r)
        type(priv_ctor_hidden_t) :: r
        r%h = 1
    end function
    function priv_ctor_new(n, m) result(r)
        integer, intent(in) :: n, m
        type(priv_ctor_overloaded_t) :: r
        r%n = n + m
    end function
end module

module priv_ctor_reexport_mod
    use priv_ctor_mod, only: priv_ctor_t
    implicit none
    private
    public :: priv_ctor_t
end module

module priv_ctor_ext_mod
    use priv_ctor_mod, only: priv_ctor_base_t, priv_ctor_t
    implicit none
    type, extends(priv_ctor_base_t) :: priv_ctor_ext_t
        integer :: y
    end type
    type :: priv_ctor_wrapper_t
        type(priv_ctor_t) :: inner = priv_ctor_t(a=1)  ! {Error} component 'a' of type 'priv_ctor_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    end type
contains
    subroutine priv_ctor_ext_construct(e)
        type(priv_ctor_ext_t), intent(out) :: e
        e = priv_ctor_ext_t(x=1, y=2)  ! {Error} component 'x' of type 'priv_ctor_base_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
        e = priv_ctor_ext_t(1, 2)  ! {Error} component 'x' of type 'priv_ctor_base_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    end subroutine
end module

subroutine priv_ctor_use()
    use priv_ctor_mod
    implicit none
    type(priv_ctor_t) :: v
    type(priv_ctor_mixed_t) :: m
    type(priv_ctor_child_t) :: ch
    type(priv_ctor_overloaded_t) :: o
    v = priv_ctor_t(a=1)  ! {Error} component 'a' of type 'priv_ctor_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    ! {Error} component 'a' of type 'priv_ctor_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    v = priv_ctor_t(1, 2)  ! {Error} component 'b' of type 'priv_ctor_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    m = priv_ctor_mixed_t(5, 6)  ! {Error} component 'q' of type 'priv_ctor_mixed_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    ch = priv_ctor_child_t(priv_ctor_hidden_t=priv_ctor_hidden_value(), c=1)  ! {Error} parent component 'priv_ctor_hidden_t' of type 'priv_ctor_child_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    ch = priv_ctor_child_t(priv_ctor_hidden_var, 1)  ! {Error} parent component 'priv_ctor_hidden_t' of type 'priv_ctor_child_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    o = priv_ctor_overloaded_t(n=1)  ! {Error} component 'n' of type 'priv_ctor_overloaded_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
    ! No error: the generic interface accepts these arguments, and the
    ! private component that is not given has a default initialization.
    o = priv_ctor_overloaded_t(1, 2)
    m = priv_ctor_mixed_t(p=5)
end subroutine

subroutine priv_ctor_renamed()
    use priv_ctor_mod, only: priv_ctor_renamed_t => priv_ctor_t
    implicit none
    type(priv_ctor_renamed_t) :: v
    v = priv_ctor_renamed_t(a=1)  ! {Error} component 'a' of type 'priv_ctor_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
end subroutine

subroutine priv_ctor_reexported()
    use priv_ctor_reexport_mod
    implicit none
    type(priv_ctor_t), parameter :: w = priv_ctor_t(1)  ! {Error} component 'a' of type 'priv_ctor_t' is private and cannot be specified in a structure constructor outside module 'priv_ctor_mod'
end subroutine
