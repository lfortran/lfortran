module intrinsics_238_module
implicit none

contains

subroutine test_diag_int16()

integer, parameter :: n = 4
integer :: a(n, n), e(n,n)

a = reshape([1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16], [n, n])

e = diag1(diag(a))

print *, sum(a)
if (sum(a) /= 136) error stop

contains

function diag(A) result(res)
integer, intent(in) :: A(:,:)
integer :: res(minval(shape(A)))
integer :: i
do i = 1, minval(shape(A))
    res(i) = A(i, i)
end do
end function diag

function diag1(v) result(res)
integer, intent(in) :: v(:)
integer :: res(size(v),size(v))
integer :: i
res = 0
do i = 1, size(v)
    res(i, i) = v(i)
end do
end function diag1

end subroutine test_diag_int16

end module

program intrinsics_238
use intrinsics_238_module
implicit none

call test_diag_int16()

end program
