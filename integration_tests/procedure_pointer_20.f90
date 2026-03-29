module procedure_pointer_component_mod
    implicit none

    type :: arg
        integer :: dummy
    end type arg

    type :: method
        integer :: nargs
        procedure(), nopass, pointer :: f => null()
        procedure(), nopass, pointer :: caller => null()
        type(arg), allocatable :: args(:)
    end type method

    interface method
        module procedure :: method_create, method_create_0
    end interface

contains

    function method_create(nargs, f) result(that)
        integer, intent(in) :: nargs
        procedure() :: f
        type(method) :: that

        that%f => f
        that%nargs = nargs
        allocate(that%args(nargs))
    end function method_create

    function method_create_0(f, caller) result(that)
        procedure() :: f
        procedure(), optional :: caller
        type(method) :: that

        that%f => f
        that%nargs = 0
        allocate(that%args(0))
        if (present(caller)) then
            that%caller => caller
        end if
    end function method_create_0

end module procedure_pointer_component_mod

program procedure_pointer_20
    use procedure_pointer_component_mod, only: method
    implicit none

    type(method) :: m1

    m1 = method(0, sub_a)

    if (m1%nargs /= 0) error stop

contains

    subroutine sub_a()
    end subroutine sub_a

end program procedure_pointer_20
