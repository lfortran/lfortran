! A name declared only with a scalar type and referenced with an argument
! list is an external function with an implicit interface (#16081)
program implicit_interface_123
    implicit none
    real :: x(4)
    integer :: l
    x = [3.0, 9.0, -1.0, 5.0]
    call find_max(4, x, l)
    if (l /= 2) error stop
end program implicit_interface_123

subroutine find_max(n, x, l)
    implicit none
    integer :: n, l, imax
    real :: x(n)
    l = imax(n, x)
end subroutine

integer function imax(n, x)
    implicit none
    integer :: n
    real :: x(n)
    imax = maxloc(x, 1)
end function
