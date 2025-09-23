program scalar_allocation_check_01
    implicit none

    integer, allocatable :: x
    integer, allocatable :: z
    integer :: y


    y = x + z
end program
