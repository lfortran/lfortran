program array_section_42
    ! Contiguous and non-contiguous sections of a 2-D fixed-size array
    ! passed to explicit-shape dummies
    implicit none
    integer :: a(3, 4), e(3, 4), i, j
    do j = 1, 4
        do i = 1, 3
            a(i, j) = 10*i + j
        end do
    end do
    e = a

    ! Contiguous: a column, trailing columns, a part of a column
    call bump(a(:, 2), 3)
    e(:, 2) = e(:, 2) + 1
    call bump2(a(:, 3:4), 3, 2)
    e(:, 3:4) = e(:, 3:4) + 1
    call bump(a(2:3, 1), 2)
    e(2:3, 1) = e(2:3, 1) + 1
    print *, a
    if (any(a /= e)) error stop 1

    ! Not contiguous: a row, a block of partial columns
    call bump(a(1, :), 4)
    e(1, :) = e(1, :) + 1
    call bump2(a(1:2, 2:3), 2, 2)
    e(1:2, 2:3) = e(1:2, 2:3) + 1
    print *, a
    if (any(a /= e)) error stop 2
contains
    subroutine bump(x, n)
        integer, intent(in) :: n
        integer, intent(inout) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = x(i) + 1
        end do
    end subroutine

    subroutine bump2(x, m, n)
        integer, intent(in) :: m, n
        integer, intent(inout) :: x(m, n)
        integer :: i, j
        do j = 1, n
            do i = 1, m
                x(i, j) = x(i, j) + 1
            end do
        end do
    end subroutine
end program
