program arrays_109
    implicit none
    integer :: M(2)

    call my_sub(M)
    if (M(1) /= 1) error stop
    if (M(2) /= 1) error stop

contains

    function make_vec() result(C)
        integer :: C(2)
        C = 1
    end function

    subroutine my_sub(M)
        integer, intent(out) :: M(2)
        M = make_vec()
    end subroutine

end program arrays_109
