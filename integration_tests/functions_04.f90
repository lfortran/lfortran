module stdlib_int

    implicit none

    interface to_negative
        module procedure :: to_negative
    end interface to_negative

contains

    pure function to_negative(int) result(neg_int)
        integer, intent(in) :: int
        integer :: neg_int

        if (int >= 0) then
            neg_int = -int
        else
            neg_int = int
        end if

    end function to_negative

end module stdlib_int

program functions_04
    use stdlib_int
    implicit none

    integer :: int = 4
    if (to_negative(int) /= -4) error stop
    int = -4
    if (to_negative(int) /= -4) error stop

end program
