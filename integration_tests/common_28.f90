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
    integer :: t
    common /block/ t
    t = 5
    call common_28_sub()
    print *, "PASS"
end program common_28
