module array_section_13_mod
implicit none
contains
    subroutine mwe(n, x, y, M)
        integer, value :: n
        real, intent(in) :: x(n), y(n)
        real, intent(out) :: M(n, n)
        integer :: k
        do k = 1, n
            call phs(x(k), y(k), M(:, k))
        end do
    contains
        subroutine phs(xc, yc, b)
            real, intent(in) :: xc, yc
            real, intent(out) :: b(n)
            real :: r
            integer :: k, l
            do k = 1, n
                r = hypot(xc - x(k), yc - y(k))
                b(k) = r**3
            end do
        end subroutine
    end subroutine
end module array_section_13_mod

program array_section_13
use array_section_13_mod
implicit none
integer, parameter :: n = 3
real :: x(3), y(3), M(n, n)
real :: eps
eps = 1e-4

x = [1.0, 3.0, 2.0]
y = [0.0, 3.0, 0.0]

call mwe(n, x, y, M)

if (abs(M(1,1)) > eps) error stop
if (abs(M(2,2)) > eps) error stop
if (abs(M(3,3)) > eps) error stop

if (abs(M(1,2) - M(2,1)) > eps) error stop
if (abs(M(1,3) - M(3,1)) > eps) error stop
if (abs(M(2,3) - M(3,2)) > eps) error stop

if (abs(M(1,2) - 46.8721657) > eps) error stop
if (abs(M(1,3) - 1.0) > eps) error stop
if (abs(M(2,3) - 31.6227760) > eps) error stop

end program array_section_13
