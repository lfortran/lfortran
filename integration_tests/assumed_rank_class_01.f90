! A type(t) assumed-rank dummy passed on to a class(t) assumed-rank dummy
module assumed_rank_class_01_m
    implicit none
    type :: cell
        real :: w = 0.0
    end type
    type, extends(cell) :: hot_cell
        integer :: k = 7
    end type
contains
    real function total_1(x) result(s)
        class(cell), intent(in) :: x(:)
        integer :: i
        s = 0.0
        do i = 1, size(x)
            s = s + x(i)%w
        end do
        select type (x)
        type is (hot_cell)
            s = s + 1000.0
        end select
    end function

    real function total_2(x) result(s)
        class(cell), intent(in) :: x(:, :)
        integer :: i, j
        s = 0.0
        do j = 1, size(x, 2)
            do i = 1, size(x, 1)
                s = s + x(i, j)%w
            end do
        end do
    end function

    subroutine probe(c, r, n, s)
        class(cell), intent(in) :: c(..)
        integer, intent(out) :: r, n
        real, intent(out) :: s
        r = rank(c)
        n = size(c)
        s = -1.0
        select rank (c)
        rank (1)
            s = total_1(c)
        rank (2)
            s = total_2(c)
        end select
    end subroutine

    integer function rank_size(c) result(r)
        class(cell), intent(in) :: c(..)
        r = 100 * rank(c) + size(c)
    end function

    subroutine pass_on(d, r, n, s)
        type(cell), intent(in) :: d(..)
        integer, intent(out) :: r, n
        real, intent(out) :: s
        call probe(d, r, n, s)
    end subroutine

    subroutine pass_on_hot(d, r, n, s)
        type(hot_cell), intent(in) :: d(..)
        integer, intent(out) :: r, n
        real, intent(out) :: s
        call probe(d, r, n, s)
    end subroutine

    integer function pass_on_func(d) result(r)
        type(cell), intent(in) :: d(..)
        r = rank_size(d)
    end function
end module

program assumed_rank_class_01
    use assumed_rank_class_01_m
    implicit none
    type(cell) :: board(4, 2), line(3), cube(2, 3, 2), one
    type(hot_cell) :: hots(3)
    integer :: rk, sz, i
    real :: s

    board%w = 1.0
    do i = 1, 3
        line(i)%w = real(i)
    end do
    hots%w = 2.0

    call pass_on(board, rk, sz, s)
    if (rk /= 2) error stop 1
    if (sz /= 8) error stop 2
    if (abs(s - 8.0) > 1e-6) error stop 3

    call pass_on(line, rk, sz, s)
    if (rk /= 1 .or. sz /= 3) error stop 4
    if (abs(s - 6.0) > 1e-6) error stop 5

    call pass_on(line(1:3:2), rk, sz, s)
    if (rk /= 1 .or. sz /= 2) error stop 6
    if (abs(s - 4.0) > 1e-6) error stop 7

    call pass_on(board(2:3, :), rk, sz, s)
    if (rk /= 2 .or. sz /= 4) error stop 8
    if (abs(s - 4.0) > 1e-6) error stop 9

    call pass_on(one, rk, sz, s)
    if (rk /= 0 .or. sz /= 1) error stop 10

    call pass_on_hot(hots, rk, sz, s)
    if (rk /= 1 .or. sz /= 3) error stop 11
    if (abs(s - 1006.0) > 1e-6) error stop 12

    if (pass_on_func(cube) /= 312) error stop 13
    if (pass_on_func(line) /= 103) error stop 14

    print *, "OK"
end program
