module arrays_reshape_50_mod
implicit none
integer, parameter :: msrc(2,3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
end module

program arrays_reshape_50
use arrays_reshape_50_mod, only: msrc
implicit none
integer, parameter :: src(2,2) = reshape([1, 2, 3, 4], [2, 2])
integer, parameter :: s1(6) = [1, 2, 3, 4, 5, 6]
integer, parameter :: s3(2,2,2) = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
real, parameter :: rs(2,2) = reshape([1.0, 2.0, 3.0, 4.0], [2, 2])
integer, parameter :: a(2,2) = reshape(src, [2, 2], order=[2, 1])
integer, parameter :: p1(2,3) = reshape(s1, [2, 3], order=[2, 1])
integer, parameter :: p2(2,2,2) = reshape(s3, [2, 2, 2], order=[3, 1, 2])
integer, parameter :: p3(3,3) = reshape(s1, [3, 3], pad=[0, -1], order=[2, 1])
integer, parameter :: p4(3,2) = reshape(msrc, [3, 2], order=[2, 1])
real, parameter :: p5(2,2) = reshape(rs, [2, 2], order=[2, 1])
integer, parameter :: z(2) = [0, -1]
integer, parameter :: p7(3,3) = reshape(s1, [3, 3], pad=z, order=[2, 1])
integer :: b(2,2), r1(2,3), r2(2,2,2), r3(3,3), r4(3,2)
real :: r5(2,2)
real(8) :: r6(2,3)
integer :: r7(3,3), r8(3,3)

b = reshape(src, [2, 2], order=[2, 1])
r1 = reshape(s1, [2, 3], order=[2, 1])
r2 = reshape(s3, [2, 2, 2], order=[3, 1, 2])
r3 = reshape(s1, [3, 3], pad=[0, -1], order=[2, 1])
r4 = reshape(msrc, [3, 2], order=[2, 1])
r5 = reshape(rs, [2, 2], order=[2, 1])
r6 = reshape([1d0+0d0, 2d0+0d0, 3d0+0d0, 4d0+0d0], [2, 3], order=[2, 1], &
    pad=[0d0])
r7 = reshape(s1, [3, 3], pad=z, order=[2, 1])
r8 = reshape([1, 2, 3, 4, 5, 6], [3, 3], pad=z, order=[2, 1])

print *, a
print *, b
if (any(a /= reshape([1, 3, 2, 4], [2, 2]))) error stop 1
if (any(b /= reshape([1, 3, 2, 4], [2, 2]))) error stop 2
if (any(p1 /= reshape([1, 4, 2, 5, 3, 6], [2, 3]))) error stop 3
if (any(r1 /= reshape([1, 4, 2, 5, 3, 6], [2, 3]))) error stop 4
if (any(p2 /= reshape([1, 3, 5, 7, 2, 4, 6, 8], [2, 2, 2]))) error stop 5
if (any(r2 /= reshape([1, 3, 5, 7, 2, 4, 6, 8], [2, 2, 2]))) error stop 6
if (any(p3 /= reshape([1, 4, 0, 2, 5, -1, 3, 6, 0], [3, 3]))) error stop 7
if (any(r3 /= reshape([1, 4, 0, 2, 5, -1, 3, 6, 0], [3, 3]))) error stop 8
if (any(p4 /= reshape([1, 3, 5, 2, 4, 6], [3, 2]))) error stop 9
if (any(r4 /= reshape([1, 3, 5, 2, 4, 6], [3, 2]))) error stop 10
if (any(abs(p5 - reshape([1.0, 3.0, 2.0, 4.0], [2, 2])) > 1e-6)) error stop 11
if (any(abs(r5 - reshape([1.0, 3.0, 2.0, 4.0], [2, 2])) > 1e-6)) error stop 12
if (any(abs(r6 - reshape([1d0, 4d0, 2d0, 0d0, 3d0, 0d0], [2, 3])) > 1d-12)) &
    error stop 13
if (any(p7 /= reshape([1, 4, 0, 2, 5, -1, 3, 6, 0], [3, 3]))) error stop 14
if (any(r7 /= reshape([1, 4, 0, 2, 5, -1, 3, 6, 0], [3, 3]))) error stop 15
if (any(r8 /= reshape([1, 4, 0, 2, 5, -1, 3, 6, 0], [3, 3]))) error stop 16
end program
