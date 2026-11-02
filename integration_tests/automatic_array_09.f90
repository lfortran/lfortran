! Temporaries for intrinsic results and elemental expressions of
! explicit-shape arrays get the extents fixed on entry, also after a
! variable of a bound is redefined, and never a negative extent.
module automatic_array_09_m
implicit none
contains

subroutine redefined(n, m, a)
    integer :: n, m
    real :: a(n, m), w(n, m)
    real, allocatable :: t(:,:)
    w = 1
    n = 100
    m = 7
    if (any(shape(transpose(a)) /= [3, 2])) error stop 1
    if (abs(sum(transpose(a)) - 12) > 1e-6) error stop 2
    if (size(matmul(a, transpose(a))) /= 4) error stop 3
    if (any(shape(transpose(w)) /= [3, 2])) error stop 4
    if (abs(sum(a + w) - 18) > 1e-6) error stop 5
    allocate(t(3, 2))
    t = transpose(a)
    if (abs(sum(t) - 12) > 1e-6) error stop 6
end subroutine

subroutine empty(n, m)
    integer, intent(in) :: n, m
    real :: a(n, m), b(m, n)
    a = 1
    b = 2
    if (any(shape(matmul(a, b)) /= [0, 0])) error stop 7
    if (any(shape(matmul(b, a)) /= [2, 2])) error stop 8
    if (any(shape(transpose(a)) /= [2, 0])) error stop 9
    if (size(pack(a, a > 0)) /= 0) error stop 10
    if (size([a, a]) /= 0) error stop 11
end subroutine

end module

program automatic_array_09
use automatic_array_09_m
implicit none
integer :: n, m
real :: x(6)
x = 2
n = 2
m = 3
call redefined(n, m, x)
call empty(-1, 2)
print *, "ok"
end program
