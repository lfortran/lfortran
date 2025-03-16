program implied_do_loops7
    implicit none
    integer, parameter :: max_ij = 13, max_sum = 2 * max_ij ** 3
    integer :: i, j

    integer, parameter :: x1(*) = [((i**3+j**3, i = 1,j), j = 1,max_ij)]
    real, parameter :: x2(*) = [((i*3.5 + j**2.5, i = 1,j), j = 1,max_ij)]
    logical, parameter :: x3(*) = [(((i + 2) > j, i = 1,j), j = 1,max_ij)]
    logical, parameter :: x4(*) = [(((i*2.5) > (j*1.5), i = 1,j), j = 1,max_ij)] 
    print *, sum(x1)
    print *, sum(x2)
    print *, count(x3)
    print *, count(x4)
    if (sum(x1) /= 115934) error stop
    if (abs(sum(x2) - 28614.8848) > 10e-12) error stop
    if (count(x3) /= 25) error stop
    if (count(x4) /= 42) error stop
end program implied_do_loops7