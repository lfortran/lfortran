! A 2x3 matrix view and a 6 element vector view of one COMMON block, in
! array element order. Row and column sections of the matrix and strided
! sections of the vector are read and written through descriptors.
module common_59_procs
    implicit none
contains
    subroutine check_row(r, expected)
        integer, intent(in) :: r(:), expected(:)
        if (size(r) /= size(expected)) error stop "row size"
        if (any(r /= expected)) error stop "row section"
    end subroutine check_row

    subroutine add_to(r, k)
        integer, intent(inout) :: r(:)
        integer, intent(in) :: k
        r = r + k
    end subroutine add_to
end module common_59_procs

subroutine common_59_set_matrix()
    use common_59_procs, only: check_row, add_to
    implicit none
    integer :: m(2, 3), i, j
    common /common_59_mv/ m
    do j = 1, 3
        do i = 1, 2
            m(i, j) = 10 * i + j
        end do
    end do
    call check_row(m(2, :), [21, 22, 23])
    call check_row(m(1, 2:3), [12, 13])
    call check_row(m(:, 3), [13, 23])
    call add_to(m(:, 2), -100)
end subroutine common_59_set_matrix

subroutine common_59_use_vector()
    use common_59_procs, only: check_row, add_to
    implicit none
    integer :: v(6)
    common /common_59_mv/ v
    if (any(v /= [11, 21, -88, -78, 13, 23])) error stop "vector view"
    call check_row(v(2::2), [21, -78, 23])
    call add_to(v(1::2), 1000)
end subroutine common_59_use_vector

program common_59
    implicit none
    integer :: m(2, 3)
    common /common_59_mv/ m
    call common_59_set_matrix()
    call common_59_use_vector()
    print *, m
    if (any(m(1, :) /= [1011, 912, 1013])) error stop "matrix view"
    if (any(m(2, :) /= [21, -78, 23])) error stop "matrix view"
end program common_59
