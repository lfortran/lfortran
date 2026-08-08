program coarrays_38
    implicit none

    type :: t
        integer :: a[*]
        integer :: b[2, *]
    end type

    integer :: x[*]
    integer :: y(5)[*]
    integer :: z[2, 3, *]
    type(t) :: s

    integer, parameter :: cr_x = corank(x)
    integer, parameter :: cr_y = corank(y)
    integer, parameter :: cr_z = corank(z)
    integer, parameter :: cr_sa = corank(s%a)
    integer, parameter :: cr_sb = corank(s%b)

    if (cr_x /= 1) error stop
    if (cr_y /= 1) error stop
    if (cr_z /= 3) error stop
    if (cr_sa /= 1) error stop
    if (cr_sb /= 2) error stop

    if (corank(x) /= 1) error stop
    if (corank(y) /= 1) error stop
    if (corank(z) /= 3) error stop

    if (corank(x[1]) /= 1) error stop
    if (corank(y(2)[1]) /= 1) error stop
    if (corank(z[1, 2, 1]) /= 3) error stop

    if (corank(s%a) /= 1) error stop
    if (corank(s%b) /= 2) error stop
    if (corank(s%a[1]) /= 1) error stop

    if (corank(coarray=x) /= 1) error stop
    if (corank(coarray=z) /= 3) error stop
end program coarrays_38
