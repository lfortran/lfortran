module data_implied_do_07_mod
    implicit none
    integer :: i, iarx(3)
    data(iarx(i), i=1,3) / 3*-2 /
end module data_implied_do_07_mod

program data_implied_do_07
    use data_implied_do_07_mod
    implicit none
    print *, iarx
    if (iarx(1) /= -2) error stop
    if (iarx(2) /= -2) error stop
    if (iarx(3) /= -2) error stop
end program data_implied_do_07