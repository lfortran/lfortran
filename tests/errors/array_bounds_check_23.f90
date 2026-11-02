program array_bounds_check_23
    ! A section of an operand of a reduction that is accumulated in a loop
    ! reaches past the end of its array
    implicit none
    real :: a(10), b(8), s
    integer :: hi
    a = 1
    b = 2
    hi = 12
    s = sum(a(5:hi) * b)
    print *, s
end program
