program derived_types_29
    implicit none
    type t
        integer :: x
    end type t
    type(t) :: type_1
    type_1%x = 10
    call sub(type_1%x)
    print*,type_1%x
    if (type_1%x /= 4352) error stop

contains
    subroutine sub(x)
        integer, intent(out) :: x
        x = 4352
    end subroutine sub
end program
