subroutine add2(n, y)
    implicit none
    integer, intent(in) :: n
    real :: y(n)
    y = y + 2.0
end subroutine
