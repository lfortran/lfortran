program associate_11
    implicit none
    integer :: x(2)
    integer :: i, j
    x = 123
    associate(a => x(:))
        a(1) = 124
    end associate
    print *, x
    if( x(1) /= 124 ) error stop
    if( x(2) /= 123 ) error stop
end program
