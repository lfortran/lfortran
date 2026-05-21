module data_implied_do_08_mod
    implicit none
    integer :: i
    integer(4) :: kr(2,2)
    data ( kr(i,1), i=1,2 ) / 1, 0 /
    data ( kr(i,2), i=1,2 ) / 0, 1 /
end module data_implied_do_08_mod

program data_implied_do_08
    use data_implied_do_08_mod
    implicit none
    print *, kr(1,1), kr(2,1), kr(1,2), kr(2,2)
    if (kr(1,1) /= 1) error stop
    if (kr(2,1) /= 0) error stop
    if (kr(1,2) /= 0) error stop
    if (kr(2,2) /= 1) error stop
end program data_implied_do_08
