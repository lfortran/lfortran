subroutine sub(x)
    real :: x
    dimension :: x(1:*)
    x(1) = 4.39
end subroutine
    
program array_unbounded_02
    real :: y(10), w(10)
    call sub(y)
    print *, y(1)
    if (abs(y(1) - 4.39) > 1e-10) error stop
end program
    