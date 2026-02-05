subroutine common_28_sub()
    common /block/ t
    integer :: t
    integer :: i
    do, i = 1, t
        if (i < 1 .or. i > t) error stop
    end do
end subroutine

program common_28
    implicit none
    print *, "PASS"
end program common_28
