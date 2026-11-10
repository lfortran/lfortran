! A local array of an internal procedure whose bound is a (non-constant)
! variable of the host program (#2430).
program nested_vars_33
    implicit none
    integer :: dims = 4
    integer :: total

    total = 0
    call sub()
    if (total /= 10) error stop 1

    dims = 6
    call sub()
    if (total /= 21) error stop 2

contains

    subroutine sub
        integer :: x(dims)
        integer :: i
        if (size(x) /= dims) error stop 3
        do i = 1, dims
            x(i) = i
        end do
        total = sum(x)
        print *, size(x), total
    end subroutine sub

end program nested_vars_33
