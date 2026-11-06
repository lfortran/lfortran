program matmul_10
! Folding `matmul` of parameter arrays: non-1 lower bounds, zero-size
! operands, small integer kinds and complex x real (#15872)
implicit none
integer, parameter :: a(0:1,-1:1) = reshape([1,2,3,4,5,6], [2,3])
integer, parameter :: b(3,2:3) = reshape([7,8,9,10,11,12], [3,2])
integer, parameter :: c(2,2) = matmul(a, b)
integer, parameter :: v(-2:0) = [1,-1,2]
integer, parameter :: d(2) = matmul(a, v)
integer, parameter :: u(0:1) = [1,2]
integer, parameter :: e(3) = matmul(u, a)
integer, parameter :: z1(0,3) = reshape([integer::], [0,3])
integer, parameter :: zc(0,2) = matmul(z1, b)
integer, parameter :: zk1(2,0) = reshape([integer::], [2,0])
integer, parameter :: zk2(0,3) = reshape([integer::], [0,3])
integer, parameter :: zk(2,3) = matmul(zk1, zk2)
logical, parameter :: la(2,3) = reshape([.true.,.false.,.false.,.false.,.true.,.false.], [2,3])
logical, parameter :: lr(2) = matmul(la, [.false.,.true.,.true.])
integer(1), parameter :: i1(2,2) = reshape([1_1,2_1,3_1,4_1], [2,2])
integer(1), parameter :: r1(2) = matmul(i1, [1_1,2_1])
integer(2), parameter :: i2(2,2) = reshape([1_2,2_2,3_2,4_2], [2,2])
integer(2), parameter :: r2(2,2) = matmul(i2, i2)
complex(8), parameter :: ca(2,2) = reshape([(1d0,2d0),(3d0,-1d0),(0d0,1d0),(2d0,2d0)], [2,2])
complex(8), parameter :: cb(2) = [(1d0,1d0),(2d0,0d0)]
real(8), parameter :: ra(2,2) = reshape([1d0,2d0,3d0,4d0], [2,2])
complex(8), parameter :: cc(2) = matmul(ca, cb)
complex(8), parameter :: rc(2) = matmul(ra, cb)
complex(8), parameter :: cr(2) = matmul(ca, [1d0,2d0])

if (any(c /= reshape([76,100,103,136], [2,2]))) error stop
if (any(d /= [8,10])) error stop
if (any(e /= [5,11,17])) error stop
if (any(shape(zc) /= [0,2])) error stop
if (size(zc) /= 0) error stop
if (any(zk /= 0)) error stop
if (any(lr .neqv. [.true.,.false.])) error stop
if (kind(r1) /= 1) error stop
if (any(r1 /= [7_1,10_1])) error stop
if (kind(r2) /= 2) error stop
if (any(r2 /= reshape([7_2,10_2,15_2,22_2], [2,2]))) error stop
if (any(cc /= [(-1d0,5d0),(8d0,6d0)])) error stop
if (any(rc /= [(7d0,1d0),(10d0,2d0)])) error stop
if (any(cr /= [(1d0,4d0),(7d0,3d0)])) error stop
print *, c, d, e
end program
