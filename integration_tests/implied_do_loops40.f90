program implied_do_loops40
    implicit none

    integer(8) :: i
    integer(8), parameter :: expected(3) = [1_8, 2_8, 3_8]
    integer(8) :: values(3)

    values = [(i, i = 1_8, 3_8, 1)]

    if (any(values /= expected)) error stop
    print *, values
end program implied_do_loops40
