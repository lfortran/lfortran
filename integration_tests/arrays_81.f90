module arrays81sub1
implicit none

contains

subroutine sub(simi, d)
    real, intent(inout) :: simi(:, :)
    real, intent(in) :: d(:)
    real :: simi_jdrop(size(simi, 2))

    simi = outprod(matprod21(simi, d), simi_jdrop)

contains

    function outprod(x, y) result(z)
        implicit none
        real, intent(in) :: x(:)
        real, intent(in) :: y(:)
        real :: z(size(x), size(y))
        real :: sum_x_y

        sum_x_y = sum(x) + sum(y)

        z = sum_x_y

    end function outprod

    function matprod21(x, y) result(z_)
        implicit none
        real, intent(in) :: x(:, :)
        real, intent(in) :: y(:)
        real, allocatable :: z_(:)

        allocate(z_(size(x, 1)))

        z_ = matmul(x, y)

    end function matprod21

end subroutine

end module

program arrays_81
use arrays81sub1

real :: simi1(10, 5)
real :: d(5)

simi1 = 5.0
d = 2.0

call sub(simi1, d)
print *, simi1
if( any(abs(simi1 - 500.0) > 1e-8 ) ) error stop

end program
