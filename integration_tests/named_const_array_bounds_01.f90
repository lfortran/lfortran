program named_const_array_bounds_01
    implicit none

    ! 1D named constant with non-default lower bound (0:2)
    integer, parameter :: d(0:2) = [4, 5, 6]
    integer :: e1(3)
    integer :: e2(0:2)
    integer :: e3(5)

    ! 2D named constant with non-default lower bounds (0:1, -1:0)
    integer, parameter :: d2(0:1, -1:0) = reshape([10, 20, 30, 40], [2, 2])
    integer :: e4(2, 2)

    ! Direct bounds inquiries
    if (lbound(d, 1) /= 0 .or. ubound(d, 1) /= 2) error stop 1
    if (lbound(d2, 1) /= 0 .or. ubound(d2, 1) /= 1) error stop 2
    if (lbound(d2, 2) /= -1 .or. ubound(d2, 2) /= 0) error stop 3

    ! Assignment to ordinary 1-based array
    e1 = d
    if (any(e1 /= [4, 5, 6])) error stop 4

    ! Assignment to array with matching non-default lower bound
    e2 = d
    if (any(e2 /= [4, 5, 6])) error stop 5

    ! Assignment to array section
    e3 = 0
    e3(2:4) = d
    if (any(e3 /= [0, 4, 5, 6, 0])) error stop 6

    ! Assignment of 2D array
    e4 = d2
    if (e4(1, 1) /= 10 .or. e4(2, 1) /= 20 .or. &
        e4(1, 2) /= 30 .or. e4(2, 2) /= 40) error stop 7

    print *, "ok"
end program named_const_array_bounds_01
