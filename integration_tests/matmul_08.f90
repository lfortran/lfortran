program matmul_08
! matmul of parameter arrays is a constant expression (#15872)
implicit none
integer, parameter :: n = 4
real, parameter :: a(n,n) = reshape([1.,2.,0.,0., 2.,3.,4.,0., &
    0.,4.,5.,6., 0.,0.,6.,7.], [n,n])
real, parameter :: x(n) = [1.,2.,3.,4.]
real, parameter :: y(n) = matmul(a, x)
integer, parameter :: ia(2,3) = reshape([1,2,3,4,5,6], [2,3])
integer, parameter :: ib(3,2) = reshape([1,2,3,4,5,6], [3,2])
integer, parameter :: ic(2,2) = matmul(ia, ib)
integer(8), parameter :: iv(3) = matmul([1_8,2_8], reshape([1_8,2_8,3_8,4_8,5_8,6_8], [2,3]))
real(8), parameter :: m(2) = matmul(ia, [0.5d0,1.5d0,2.5d0])
logical, parameter :: la(2,2) = reshape([.true.,.false.,.false.,.true.], [2,2])
logical, parameter :: lb(2,2) = reshape([.true.,.true.,.false.,.false.], [2,2])
logical, parameter :: lc(2,2) = matmul(la, lb)
complex, parameter :: ca(2,2) = reshape([(1.,1.),(0.,1.),(2.,0.),(1.,0.)], [2,2])
complex, parameter :: cv(2) = matmul(ca, [(1.,0.),(0.,1.)])
real, parameter :: z(3) = matmul([1.5,2.5], reshape([1.,2.,3.,4.,5.,6.], [2,3]))

if (any(y /= [5., 20., 47., 46.])) error stop
if (any(y /= matmul(a, x))) error stop
if (any(ic /= reshape([22,28,49,64], [2,2]))) error stop
if (any(iv /= [5_8,11_8,17_8])) error stop
if (kind(m) /= 8) error stop
if (any(abs(m - [17.5d0,22d0]) > 1d-12)) error stop
if (any(lc .neqv. reshape([.true.,.true.,.false.,.false.], [2,2]))) error stop
if (any(cv /= [(1.,3.),(0.,2.)])) error stop
if (any(abs(z - [6.5,14.5,22.5]) > 1e-6)) error stop
print *, y
end program
