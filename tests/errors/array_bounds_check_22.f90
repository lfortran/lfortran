program array_bounds_check_22
    implicit none
    integer :: y(3, 3), i
    y = -1
    forall (i = 1:3) y(i, :) = [i, 2*i]
    print *, y
end program
