! Inquiries about the bounds of explicit-shape arrays in the bounds of other
! local arrays that are evaluated by a helper function
module shape_automatic_bound_02_m
implicit none
contains

    pure integer function total(sh)
        integer, intent(in) :: sh(:)
        total = sum(sh)
    end function

    ! Automatic arrays of rank 1 to 3, with lower bounds other than 1
    subroutine ranks(n, m)
        integer, intent(in) :: n, m
        integer :: a(n)
        real :: b(2:n, m)
        real :: c(2:n, m, 0:1)
        integer :: d(sum(shape(a)))
        integer :: e(maxval(shape(b)) + 1)
        integer :: f(product(shape(c)))
        integer :: g(size(shape(c)))
        integer :: h(sum(ubound(c)))
        integer :: i(sum(lbound(c)) + 1)
        integer(8) :: j(product(shape(b, kind=8)))
        integer :: k(total(shape(c)))
        integer :: ext, lb, ub
        ! The first dimension of `b` and `c` has zero extent if n < 2, and
        ! then the bounds 1 and 0
        ext = max(n - 1, 0)
        lb = merge(2, 1, n >= 2)
        ub = merge(n, 0, n >= 2)
        if (size(d) /= n) error stop 1
        if (size(e) /= max(ext, m) + 1) error stop 2
        if (size(f) /= 2*m*ext) error stop 3
        if (size(g) /= 3) error stop 4
        if (size(h) /= ub + m + 1) error stop 5
        if (size(i) /= lb + 1 + 0 + 1) error stop 6
        if (size(j) /= m*ext) error stop 7
        if (size(k) /= ext + m + 2) error stop 8
    end subroutine

    ! An explicit-shape dummy, and an array of zero extent
    subroutine dummy(n, x)
        integer, intent(in) :: n
        integer, intent(in) :: x(n, 2)
        integer :: z(n:n - 1)
        integer :: c(product(shape(x)))
        integer :: d(product(shape(z)) + 1)
        if (size(c) /= 2*n) error stop 9
        if (size(d) /= 1) error stop 10
    end subroutine

    ! A BLOCK, and an array of the host
    subroutine nested(n)
        integer, intent(in) :: n
        integer :: a(n, 2)
        integer :: k
        k = n + 1
        block
            integer :: z(k)
            integer :: w(product(shape(z)) * 2)
            if (size(w) /= 2*(n + 1)) error stop 11
        end block
        call inner()
    contains
        subroutine inner()
            integer :: c(product(shape(a)))
            if (size(c) /= 2*n) error stop 12
        end subroutine
    end subroutine

end module

program shape_automatic_bound_02
use shape_automatic_bound_02_m
implicit none
integer :: y(3, 2)
call ranks(4, 5)
call ranks(1, 3)
y = 0
call dummy(3, y)
call nested(3)
print *, "ok"
end program
