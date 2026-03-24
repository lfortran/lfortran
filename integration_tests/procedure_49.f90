module procedure_49_mod
    implicit none

    integer :: seen = -1

    type :: my_type
        procedure(), pointer, nopass :: caller => null()
        procedure(), pointer, nopass :: f => null()
    end type my_type

contains

    subroutine dummy_caller(func, arg)
        procedure() :: func
        integer, intent(in) :: arg
        seen = arg
    end subroutine dummy_caller

    subroutine dummy_f(arg)
        integer, intent(in) :: arg
        if (arg /= 42) error stop
    end subroutine dummy_f

    subroutine invoke(this, arg1)
        class(my_type), intent(in) :: this
        integer, intent(in) :: arg1
        call this%caller(this%f, arg1)
    end subroutine invoke

end module procedure_49_mod

program procedure_49
    use procedure_49_mod
    implicit none

    type(my_type) :: t

    t%caller => dummy_caller
    t%f => dummy_f

    call invoke(t, 42)

    if (seen /= 42) error stop
end program procedure_49
