! Explicit-shape arrays whose upper bound is below the lower bound at run time
! have zero extent.
module automatic_array_04_m
    implicit none
contains
    function init(x) result(y)
        integer, intent(in) :: x(:)
        integer :: y(size(x)-1)
        y = x(:size(x)-1)
    end function

    function grid(n) result(r)
        integer, intent(in) :: n
        integer :: r(mod(n, 5) - 1, min(n, 4))
        r = 1
    end function

    subroutine s(n)
        integer, intent(in) :: n
        integer :: a(n), b(2:n), c(n, 3)
        a = 1
        b = 1
        c = 1
        if (size(a) /= 0 .or. sum(a) /= 0) error stop 1
        if (size(b) /= 0 .or. lbound(b, 1) /= 1 .or. ubound(b, 1) /= 0) error stop 2
        if (size(c) /= 0 .or. any(shape(c) /= [0, 3])) error stop 3
        if (ubound(c, 1) /= 0 .or. ubound(c, 2) /= 3) error stop 4
        call t(a, n)
    end subroutine

    subroutine t(a, n)
        integer, intent(in) :: n
        integer, intent(in) :: a(n)
        if (size(a) /= 0 .or. ubound(a, 1) /= 0) error stop 5
    end subroutine
end module

program automatic_array_04
    use automatic_array_04_m
    implicit none
    integer :: x(3)
    x = [1, 2, 3]
    if (size(init(init([1]))) /= 0) error stop 6
    if (size(init(init(init(x)))) /= 0) error stop 7
    if (any(init(init(x)) /= [1])) error stop 8
    if (size(grid(-2)) /= 0 .or. any(shape(grid(-2)) /= [0, 0])) error stop 9
    if (any(shape(grid(3)) /= [2, 3])) error stop 10
    call s(-3)
    call s(0)
    print *, "ok"
end program
