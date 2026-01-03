module arrays_92_mod
    implicit none
    integer, parameter :: ints(0:9) = [11, 12, 13, 14, 15, 16, 17, 18, 19, 20]
    integer :: e = 3
end module arrays_92_mod

program arrays_92
    use arrays_92_mod, only : e, ints
    integer :: x
    x = ints(e)
    print *, "x: ", x
    if (x /= 14) error stop
end program arrays_92
