! x(2) covers n and a(1) of common_52_left, and y covers a(2).
module common_52_right
    implicit none
    integer(4) :: x(2), y
    common /common_52_blk/ x, y
    integer(4) :: v(6)
    common /common_52_mat/ v
contains
    subroutine set_values()
        x = [7, 11]
        y = 13
    end subroutine set_values

    subroutine fill_vector()
        v = [1, 2, 3, 4, 5, 6]
    end subroutine fill_vector
end module common_52_right
