program arrays_139
    ! A local array with 1e7 elements must not overflow the stack (#5427)
    implicit none
    integer, parameter :: ten8 = 1e7
    real :: tab(ten8)
    real :: s
    call random_number(tab)
    s = sum(tab)
    print *, 'sum= ', s, '  expected= ', 0.5*ten8
    if (size(tab) /= 10000000) error stop
    if (any(tab < 0.0) .or. any(tab >= 1.0)) error stop
    if (abs(s - 0.5*ten8) > 0.01*ten8) error stop
end program arrays_139
