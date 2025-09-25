program implied_do_loops7
    implicit none
    integer, parameter :: max_ij = 13, max_sum = 2 * max_ij ** 3
    integer :: i, j

    integer, parameter :: x1(*) = [((i**3+j**3, i = 1,j), j = 1,max_ij)]
    real, parameter :: x2(*) = [((i*3.5 + j**2.5, i = 1,j), j = 1,max_ij)]
    logical, parameter :: x3(*) = [(((i + 2) > j, i = 1,j), j = 1,max_ij)]
    logical, parameter :: x4(*) = [(((i*2.5) > (j*1.5), i = 1,j), j = 1,max_ij)] 
    !Character Implied Do Loop Test with Parameter Array
    character(len=2),parameter :: char_array(2) = (/(('AB'), i=1,2) /) 
    !Check variable length character array assignment
    character(len=10),parameter :: char_array2(2) = (/(('ABCDEFGH'), i=1,2) /)
    character(len=2),parameter :: char_array3(2) = (/(('ABCDEFGH'), i=1,2) /)
    print *, sum(x1)
    print *, sum(x2)
    print *, count(x3)
    print *, count(x4)
    print *, char_array
    print *, char_array2, char_array3
    if (sum(x1) /= 115934) error stop
    if (abs(sum(x2) - 28614.8848) > 10e-12) error stop
    if (count(x3) /= 25) error stop
    if (count(x4) /= 42) error stop
    if (char_array(1) /= 'AB' .or. char_array(2) /= 'AB') error stop
    if (char_array2(1) /= 'ABCDEFGH' .or. char_array2(2) /= 'ABCDEFGH') error stop
    if (char_array3(1) /= 'AB' .or. char_array3(2) /= 'AB') error stop
end program implied_do_loops7