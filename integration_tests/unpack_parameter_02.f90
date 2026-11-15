program unpack_parameter_02
! unpack with a rank-2 (and rank-3) mask and constant arguments: the folded
! result has the shape of the mask (#17310)
implicit none
integer, parameter :: qp = 16
logical, parameter :: m(2,3) = reshape([.true., .false., .false., &
    .true., .false., .true.], [2,3])
logical, parameter :: m3(2,1,2) = reshape([.false., .true., .true., .false.], [2,1,2])
real(8), parameter :: f8(2,3) = reshape([1, 2, 3, 4, 5, 6], [2,3])
real(qp), parameter :: f16(2,3) = reshape([1, 2, 3, 4, 5, 6], [2,3])
integer(1), parameter :: f1(2,3) = reshape([1_1, 2_1, 3_1, 4_1, 5_1, 6_1], [2,3])
character(2), parameter :: fc(2,3) = reshape(['a1', 'b2', 'c3', 'd4', 'e5', 'f6'], [2,3])

real(8), parameter :: p8s(2,3) = unpack([10d0, 20d0, 30d0], m, -1d0)
real(8), parameter :: p8a(2,3) = unpack([10d0, 20d0, 30d0], m, f8)
real(qp), parameter :: p16s(2,3) = unpack([10.0_qp, 20.0_qp, 30.0_qp], m, -1.0_qp)
real(qp), parameter :: p16a(2,3) = unpack([10.0_qp, 20.0_qp, 30.0_qp], m, f16)
integer(1), parameter :: p1s(2,3) = unpack([10_1, 20_1, 30_1], m, -1_1)
integer(1), parameter :: p1a(2,3) = unpack([10_1, 20_1, 30_1], m, f1)
character(2), parameter :: pcs(2,3) = unpack(['xx', 'yy', 'zz'], m, '--')
character(2), parameter :: pca(2,3) = unpack(['xx', 'yy', 'zz'], m, fc)
integer, parameter :: p3(2,1,2) = unpack([7, 8], m3, 0)

real(8) :: r8s(2,3), r8a(2,3)
real(qp) :: r16s(2,3), r16a(2,3)
integer(1) :: r1s(2,3), r1a(2,3)
character(2) :: rcs(2,3), rca(2,3)
integer :: r3(2,1,2)

r8s = unpack([10d0, 20d0, 30d0], m, -1d0)
r8a = unpack([10d0, 20d0, 30d0], m, f8)
r16s = unpack([10.0_qp, 20.0_qp, 30.0_qp], m, -1.0_qp)
r16a = unpack([10.0_qp, 20.0_qp, 30.0_qp], m, f16)
r1s = unpack([10_1, 20_1, 30_1], m, -1_1)
r1a = unpack([10_1, 20_1, 30_1], m, f1)
rcs = unpack(['xx', 'yy', 'zz'], m, '--')
rca = unpack(['xx', 'yy', 'zz'], m, fc)
r3 = unpack([7, 8], m3, 0)

if (any(shape(unpack([10d0, 20d0, 30d0], m, -1d0)) /= [2, 3])) error stop 1
if (any(shape(unpack([7, 8], m3, 0)) /= [2, 1, 2])) error stop 2
if (size(unpack(['xx', 'yy', 'zz'], m, fc), 2) /= 3) error stop 3

if (any(p8s /= reshape([10d0, -1d0, -1d0, 20d0, -1d0, 30d0], [2,3]))) error stop 4
if (any(p8a /= reshape([10d0, 2d0, 3d0, 20d0, 5d0, 30d0], [2,3]))) error stop 5
if (any(p16s /= reshape([10.0_qp, -1.0_qp, -1.0_qp, 20.0_qp, -1.0_qp, 30.0_qp], [2,3]))) error stop 6
if (any(p16a /= reshape([10.0_qp, 2.0_qp, 3.0_qp, 20.0_qp, 5.0_qp, 30.0_qp], [2,3]))) error stop 7
if (any(p1s /= reshape([10_1, -1_1, -1_1, 20_1, -1_1, 30_1], [2,3]))) error stop 8
if (any(p1a /= reshape([10_1, 2_1, 3_1, 20_1, 5_1, 30_1], [2,3]))) error stop 9
if (any(pcs /= reshape(['xx', '--', '--', 'yy', '--', 'zz'], [2,3]))) error stop 10
if (any(pca /= reshape(['xx', 'b2', 'c3', 'yy', 'e5', 'zz'], [2,3]))) error stop 11
if (any(p3 /= reshape([0, 7, 8, 0], [2,1,2]))) error stop 12
if (p16a(2,2) /= 20.0_qp .or. p8s(1,3) /= -1d0 .or. pca(1,3) /= 'e5') error stop 13

if (any(r8s /= p8s) .or. any(r8a /= p8a)) error stop 14
if (any(r16s /= p16s) .or. any(r16a /= p16a)) error stop 15
if (any(r1s /= p1s) .or. any(r1a /= p1a)) error stop 16
if (any(rcs /= pcs) .or. any(rca /= pca)) error stop 17
if (any(r3 /= p3)) error stop 18
print *, p8a
print *, p1a
print *, pca
print *, p3
end program
