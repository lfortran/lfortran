subroutine data_section1()
    implicit none
    integer :: x(4)
    data x(1) /1/
    data x(3:5) /3, 4, 5/
end subroutine data_section1
