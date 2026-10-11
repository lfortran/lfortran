! One view of /common_52_blk/: a scalar followed by an array. The modules
! common_52_left, common_52_right and common_52_swap do not use each other;
! each one knows only its own layout of the block.
module common_52_left
    implicit none
    integer(4) :: n, a(2)
    common /common_52_blk/ n, a
    ! /common_52_mat/ is a 2x3 matrix here and a vector in common_52_right:
    ! its elements are in array element order, column by column.
    integer(4) :: m(2, 3)
    common /common_52_mat/ m
contains
    subroutine fill_matrix()
        integer :: i, j
        do j = 1, 3
            do i = 1, 2
                m(i, j) = 10 * i + j
            end do
        end do
    end subroutine fill_matrix
end module common_52_left
