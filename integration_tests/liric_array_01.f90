program liric_array_01
    implicit none
    integer :: x(2)
    x(1) = 3
    x(2) = 7
    if (x(1) /= 3) error stop
    if (x(2) /= 7) error stop
end program
