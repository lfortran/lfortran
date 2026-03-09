module mod
    implicit none
    integer :: i, iarx(3)
    data(iarx(i), i=1,3) / 3*-2 /
end module mod

program main
    use mod
    implicit none
    print *, iarx
    if (iarx(1) /= -2) error stop
    if (iarx(2) /= -2) error stop
    if (iarx(3) /= -2) error stop
end program main
