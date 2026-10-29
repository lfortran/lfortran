! Function results assigned to an actual argument, elemental references
! with the same actual for two dummy arguments, and MVBITS with TO the same
! variable as FROM (F2018 16.9.135), with noalias dummy arguments under
! --fast.
module noalias_dummy_03_m
implicit none
contains

function shift_add(x) result(r)
    real, intent(in) :: x(:)
    real :: r(size(x))
    integer :: i
    do i = 1, size(x)
        r(i) = x(i) + x(size(x) - i + 1)
    end do
end function

function twice(x, n) result(r)
    integer, intent(in) :: n
    real, intent(in) :: x(n)
    real :: r(n)
    integer :: i
    do i = 1, n
        r(i) = 2*x(n - i + 1)
    end do
end function

elemental subroutine add_into(a, b, c)
    real, intent(in) :: a, b
    real, intent(out) :: c
    c = a + b
end subroutine

elemental real function mul(a, b)
    real, intent(in) :: a, b
    mul = a*b
end function

end module

program noalias_dummy_03
use noalias_dummy_03_m
implicit none
real :: a(4), c(4)
integer :: i, j

a = [1., 2., 3., 4.]
a = shift_add(a)
if (any(abs(a - [5., 5., 5., 5.]) > 1e-6)) error stop 1

a = [1., 2., 3., 4.]
a = twice(a, 4)
if (any(abs(a - [8., 6., 4., 2.]) > 1e-6)) error stop 2

a = [1., 2., 3., 4.]
call add_into(a, a, c)
if (any(abs(c - [2., 4., 6., 8.]) > 1e-6)) error stop 3
c = mul(a, a)
if (any(abs(c - [1., 4., 9., 16.]) > 1e-6)) error stop 4

i = int(b'1011', kind(i))
call mvbits(i, 0, 4, i, 4)
if (i /= int(b'10111011', kind(i))) error stop 5
j = 5
call mvbits(j, 0, 3, j, 1)
if (j /= 11) error stop 6
print *, "ok"
end program
