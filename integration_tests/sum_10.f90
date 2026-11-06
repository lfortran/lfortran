program sum_10
! Unary `.not.` and complex unary minus inside an elemental intrinsic that is
! the argument of a reduction lowered to an accumulation loop.
implicit none
real :: x(4)
logical :: m(4)
complex :: z(5)
integer :: i

x = [1.0, -2.0, 3.0, -4.0]
m = x > 0
do i = 1, 5
    z(i) = cmplx(real(i), -real(i) / 2)
end do

if (abs(sum(merge(x, 2*x, .not. m)) - 2.0) > 1e-5) error stop 1
if (abs(sum(merge(x, 2*x, .not. (x > 0))) - 2.0) > 1e-5) error stop 2
if (count(.not. m) /= 2) error stop 3
if (abs(sum(abs(-z)) - 16.7705098) > 1e-4) error stop 4
if (abs(sum(real(exp(-z))) - check_real_exp(z)) > 1e-5) error stop 5
if (abs(sum(exp(-z)) - check_exp(z)) > 1e-5) error stop 6
if (abs(sum_exp_dummy(5, z) - check_exp(z)) > 1e-5) error stop 7
if (abs(sum_abs_dummy(5, z) - 16.7705098) > 1e-4) error stop 8
if (abs(sum_merge_dummy(4, x, m) - 2.0) > 1e-5) error stop 9
print *, "ok"

contains

    complex function sum_exp_dummy(n, w) result(r)
        integer, intent(in) :: n
        complex, intent(in) :: w(n)
        r = sum(exp(-w))
    end function

    real function sum_abs_dummy(n, w) result(r)
        integer, intent(in) :: n
        complex, intent(in) :: w(n)
        r = sum(abs(-w))
    end function

    real function sum_merge_dummy(n, a, mask) result(r)
        integer, intent(in) :: n
        real, intent(in) :: a(n)
        logical, intent(in) :: mask(n)
        r = sum(merge(a, 2*a, .not. mask))
    end function

    real function check_real_exp(w) result(r)
        complex, intent(in) :: w(:)
        integer :: j
        r = 0
        do j = 1, size(w)
            r = r + real(exp(-w(j)))
        end do
    end function

    complex function check_exp(w) result(r)
        complex, intent(in) :: w(:)
        integer :: j
        r = 0
        do j = 1, size(w)
            r = r + exp(-w(j))
        end do
    end function

end program
