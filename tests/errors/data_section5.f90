subroutine data_section5(n)
    implicit none
    integer :: n, x(3), y(5), i
    data x(1:3), (y(i), i=1,n) /1, 2/
end subroutine data_section5
