program intrinsics_510
! dble, float and dfloat of an array are elemental, like real(a, kind):
! their result has the shape of the array also when it is allocatable, a
! pointer or a named constant
implicit none
integer, parameter :: pb(5:6) = [1, 2]
real(8), parameter :: pd(2) = dble(pb)
real(4), parameter :: pf(2) = float(pb)
real(8), parameter :: pdf(2) = dfloat(pb)
integer, allocatable :: ia(:)
real(4), allocatable :: ra(:)
complex(4), allocatable :: za(:)
integer, pointer :: ip(:)
real(8) :: d(4)
real(4) :: s(4)

if (any(abs(pd - [1d0, 2d0]) > 1d-12)) error stop
if (any(abs(pf - [1.0, 2.0]) > 1e-6)) error stop
if (any(abs(pdf - [1d0, 2d0]) > 1d-12)) error stop

d = 0
d(1:2) = dble(pb)
if (any(abs(d - [1d0, 2d0, 0d0, 0d0]) > 1d-12)) error stop

allocate(ia(-2:1))
ia = [1, 2, 3, 4]
d = dble(ia)
if (any(abs(d - [1d0, 2d0, 3d0, 4d0]) > 1d-12)) error stop
s = float(ia)
if (any(abs(s - [1.0, 2.0, 3.0, 4.0]) > 1e-6)) error stop
d = 0
d = dfloat(ia)
if (any(abs(d - [1d0, 2d0, 3d0, 4d0]) > 1d-12)) error stop
if (size(dble(ia)) /= 4) error stop
if (abs(sum(dble(ia)) - 10d0) > 1d-12) error stop
if (abs(sum(float(ia)) - 10.0) > 1e-6) error stop

allocate(ra(0:3))
ra = [0.5, 1.5, 2.5, 3.5]
d = dble(ra)
if (any(abs(d - [0.5d0, 1.5d0, 2.5d0, 3.5d0]) > 1d-12)) error stop

allocate(za(4))
za = [(1.0, 2.0), (3.0, 4.0), (5.0, 6.0), (7.0, 8.0)]
d = dble(za)
if (any(abs(d - [1d0, 3d0, 5d0, 7d0]) > 1d-12)) error stop

allocate(ip(3:6))
ip = [5, 6, 7, 8]
d = dble(ip)
if (any(abs(d - [5d0, 6d0, 7d0, 8d0]) > 1d-12)) error stop
if (abs(maxval(float(ip)) - 8.0) > 1e-6) error stop
deallocate(ip)

print *, "ok"
end program
