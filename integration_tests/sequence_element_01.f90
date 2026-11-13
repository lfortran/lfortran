! An array element passed to an explicit-shape or assumed-size dummy is
! the element sequence that starts at that element. The dummy may write
! those elements, and a rank-1 sequence may fill a rank-2 dummy.
program sequence_element_01
implicit none
integer :: a(6)
integer :: got
a = [1, 2, 3, 4, 5, 6]
call add_n(a(2), 3, 10)
if (a(1) /= 1 .or. a(2) /= 12 .or. a(3) /= 13 .or. a(4) /= 14) error stop 1
if (a(5) /= 5 .or. a(6) /= 6) error stop 2
got = sum_star(a(3), 3)
if (got /= 13 + 14 + 5) error stop 3
call fill_2d(a(1), 2, 2)
if (a(1) /= 7 .or. a(2) /= 8 .or. a(3) /= 9 .or. a(4) /= 10) error stop 4
if (a(5) /= 5 .or. a(6) /= 6) error stop 5

contains

    subroutine add_n(x, n, k)
        integer, intent(in) :: n, k
        integer, intent(inout) :: x(n)
        integer :: i
        do i = 1, n
            x(i) = x(i) + k
        end do
    end subroutine add_n

    integer function sum_star(x, n)
        integer, intent(in) :: n
        integer, intent(in) :: x(*)
        integer :: i
        sum_star = 0
        do i = 1, n
            sum_star = sum_star + x(i)
        end do
    end function sum_star

    subroutine fill_2d(x, m, n)
        integer, intent(in) :: m, n
        integer, intent(out) :: x(m, n)
        integer :: i, j, k
        k = 7
        do j = 1, n
            do i = 1, m
                x(i, j) = k
                k = k + 1
            end do
        end do
    end subroutine fill_2d

end program sequence_element_01
