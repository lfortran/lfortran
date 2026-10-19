! A do loop bound that calls an internal function which reads a variable of the
! host (#14757).
program nested_vars_27
    implicit none
    integer :: n, i, s
    n = 3
    s = 0
    do i = 1, f()
        s = s + i
        n = n + 1
    end do
    if (s /= 6) error stop 1
    if (n /= 6) error stop 2
    print *, s, n
contains
    integer function f()
        f = n
    end function
end program
