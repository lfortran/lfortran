pure function linspace_n_1_cdp_cdp(x, y, n) result(res)
integer, intent(in) :: n

complex :: res(n)

real, intent(in) :: x(n)
real, intent(in) :: y(n)

res = cmplx(x, y)
end function linspace_n_1_cdp_cdp

program complex_17
implicit none
integer :: i
real :: x(10), y(10)
complex :: z(10)
interface
    pure function linspace_n_1_cdp_cdp(x, y, n) result(res)
    integer, intent(in) :: n
    complex :: res(n)
    real, intent(in) :: x(n)
    real, intent(in) :: y(n)
    end function linspace_n_1_cdp_cdp
end interface

x = 1.0
y = 2.0

z = linspace_n_1_cdp_cdp(x, y, 10)
print *, z
print *, abs(z)
do i = 1, 10
    if (abs(abs(z(i)) - 2.236068) > 1e-8) error stop
end do
end program
