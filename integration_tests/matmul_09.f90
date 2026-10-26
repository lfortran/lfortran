program matmul_09
! Intentionally non-conforming: the integer products overflow in a constant
! expression. Checks that folding `matmul` does not crash and wraps in the
! result kind, like LFortran's other folding (huge(1)+1, sum, dot_product)
! (#15872). GFortran rejects the overflow unless given -fno-range-check.
implicit none
integer, parameter :: a(1,1) = reshape([100000], [1,1])
integer, parameter :: x(1) = [100000]
integer(1), parameter :: b(1,2) = reshape([100_1, 100_1], [1,2])
integer(1), parameter :: v(2) = [1_1, 1_1]
integer :: y(1)
integer(1) :: w(1)
y = matmul(a, x)
w = matmul(b, v)
print *, y, w
if (y(1) /= 1410065408) error stop
if (w(1) /= -56_1) error stop
end program
