! A noncontiguous section of a fixed-size array, associated with an
! explicit-shape or assumed-size dummy, gives the dummy the sequence of its
! own elements in array element order: the procedure sees and defines only
! those, also for a reversed section, a matrix row and a 2-D block, and a
! contiguous section is passed in place.
module array_section_37_m
    implicit none
contains
    subroutine scale_explicit(n, y)
        integer, intent(in) :: n
        real(8), intent(inout) :: y(n)
        y = 2 * y
    end subroutine scale_explicit

    subroutine fill_assumed_size(n, y)
        integer, intent(in) :: n
        real(8), intent(out) :: y(*)
        integer :: i
        do i = 1, n
            y(i) = i
        end do
    end subroutine fill_assumed_size

    subroutine set_block(m, n, a)
        integer, intent(in) :: m, n
        real(8) :: a(m, n)
        integer :: i, j
        do j = 1, n
            do i = 1, m
                a(i, j) = 10 * i + j
            end do
        end do
    end subroutine set_block

    real(8) function ends(n, y)
        integer, intent(in) :: n
        real(8), intent(in) :: y(n)
        ends = y(1) + y(n)
    end function ends
end module array_section_37_m

program array_section_37
    use array_section_37_m
    implicit none
    real(8) :: ys(6), m(3, 3)
    integer :: i

    ys = [1d0, 10d0, 2d0, 20d0, 3d0, 30d0]
    call scale_explicit(3, ys(1::2))
    if (any(ys /= [2d0, 10d0, 4d0, 20d0, 6d0, 30d0])) error stop 1
    call scale_explicit(3, ys(6:2:-2))
    if (any(ys /= [2d0, 20d0, 4d0, 40d0, 6d0, 60d0])) error stop 2
    call fill_assumed_size(3, ys(2::2))
    if (any(ys /= [2d0, 1d0, 4d0, 2d0, 6d0, 3d0])) error stop 3
    if (ends(3, ys(5:1:-2)) /= 8) error stop 4
    call scale_explicit(2, ys(3:4))
    if (any(ys /= [2d0, 1d0, 8d0, 4d0, 6d0, 3d0])) error stop 5

    m = reshape([(real(i, 8), i = 1, 9)], [3, 3])
    call scale_explicit(3, m(2, :))
    if (any(m(2, :) /= [4d0, 10d0, 16d0])) error stop 6
    if (any(m(1, :) /= [1d0, 4d0, 7d0]) .or. any(m(3, :) /= [3d0, 6d0, 9d0])) error stop 7
    m = 0
    call set_block(2, 2, m(2:3, 2:3))
    if (any(m(2:3, 2:3) /= reshape([11d0, 21d0, 12d0, 22d0], [2, 2]))) error stop 8
    if (any(m(1, :) /= 0) .or. any(m(:, 1) /= 0)) error stop 9
    print '(a)', 'array_section_37 ok'
end program array_section_37
