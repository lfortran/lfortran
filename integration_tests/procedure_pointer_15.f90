program procedure_pointer_15
    implicit none
    procedure(), pointer :: mysub => null()
    integer :: val

    val = 0
    mysub => set_forty_two
    call mysub(val)
    if (val /= 42) error stop

    val = 0
    mysub => set_seven
    call mysub(val)
    if (val /= 7) error stop

    print *, "procedure_pointer_15: PASS"
contains
    subroutine set_forty_two(v)
        integer, intent(out) :: v
        v = 42
    end subroutine

    subroutine set_seven(v)
        integer, intent(out) :: v
        v = 7
    end subroutine
end program
