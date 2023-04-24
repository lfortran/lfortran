program example_lmder1
implicit none
integer, parameter :: wp = 8
integer, parameter :: n = 3
integer, parameter :: m = 15
integer :: i
real(wp) :: x(n), fvec(m)
x = 1
call fcn(m, n, x, fvec)
print *, fvec
do i = 1, m
    if ((abs(fvec(i)) - 1.0) > 1e-6) error stop
end do

contains

    subroutine check_deriv()
        call fcn2(m, n, x, fvec)
    end subroutine check_deriv

    subroutine fcn2(m, n, x, fvec)
        integer, intent(in) :: m
        integer, intent(in) :: n
        real(wp), intent(in) :: x(n)
        real(wp), intent(inout) :: fvec(m)
    end subroutine fcn2

    subroutine fcn(m, n, x, fvec)
        integer, intent(in) :: m
        integer, intent(in) :: n
        real(wp), intent(in) :: x(n)
        real(wp), intent(out) :: fvec(m)
        fvec = x(1)
    end subroutine fcn

end program
