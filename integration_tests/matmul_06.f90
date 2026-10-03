module matmul_06_mod
implicit none
contains
pure function diag(v) result(res)
    real, intent(in) :: v(:)
    real :: res(size(v), size(v))
    integer :: i
    res = 0.0
    do i = 1, size(v)
        res(i, i) = v(i)
    end do
end function
end module

program matmul_06
use matmul_06_mod, only: diag
implicit none
real :: s(2), vt(2,2), r(2,2)
s = [2.0, 3.0]
vt = reshape([1.0, 0.0, 0.0, 1.0], [2, 2])
r = matmul(diag(s), vt)
print *, r(1,1), r(2,1), r(1,2), r(2,2)
if (abs(r(1,1) - 2.0) > 1e-6) error stop
if (abs(r(2,1) - 0.0) > 1e-6) error stop
if (abs(r(1,2) - 0.0) > 1e-6) error stop
if (abs(r(2,2) - 3.0) > 1e-6) error stop
end program
