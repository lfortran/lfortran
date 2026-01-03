subroutine sub(x, z)
    real :: x(1:*), z(10)
    x(1) = 4.29
    print *, x(1)
end subroutine
    
program array_unbounded_01
    real :: y(10), w(10)
    call sub(y, w)
    print *, y(1)
    if (abs(y(1) - 4.29) > 1e-10) error stop
end program
