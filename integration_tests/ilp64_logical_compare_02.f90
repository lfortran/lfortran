program ilp64_logical_compare_02
    implicit none
    integer :: v(6)
    integer, allocatable :: a(:)
    integer(4) :: s(4)
    logical :: b

    v = 0
    b = any(v /= 1)
    if (.not. b) error stop
    b = any(v /= [0, 0, 7, 8, 9, 0])
    if (.not. b) error stop
    if (all(v == [0, 0, 7, 8, 9, 0])) error stop
    if (count(v /= 1) /= 6) error stop

    v = [1, 2, 3, 4, 5, 6]
    if (any(1 > v)) error stop
    if (.not. all(v >= 1)) error stop
    if (count(v > 3) /= 3) error stop
    if (count(v(2:5) > 2) /= 3) error stop
    if (.not. any(v(1:6:2) == 3)) error stop

    allocate(a(5))
    a = [1, 1, 2, 3, 5]
    if (.not. any(a == 5)) error stop
    if (.not. all(a > 0)) error stop
    if (count(a == 1) /= 2) error stop

    s = [1, 2, 3, 4]
    if (count(s > 2) /= 2) error stop
    if (.not. any(s == 4_4)) error stop

    call check(v, 6)
    print *, "All tests passed"

contains

    subroutine check(x, n)
        integer, intent(in) :: n
        integer, intent(in) :: x(n)
        if (count(x > 1) /= 5) error stop
        if (.not. any(x == n)) error stop
        if (.not. all(x <= n)) error stop
    end subroutine

end program
