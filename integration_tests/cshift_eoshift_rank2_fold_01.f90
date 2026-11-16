program cshift_eoshift_rank2_fold_01
! cshift and eoshift of rank-2 and rank-3 constant arrays are folded in a
! PARAMETER initializer, along any dimension and with array shifts (#17250)
implicit none
integer, parameter :: m(2,3) = reshape([10, 20, 30, 40, 50, 60], [2, 3])
integer, parameter :: t(2,2,2) = reshape([1, 2, 3, 4, 5, 6, 7, 8], [2, 2, 2])
integer(8), parameter :: h = huge(1_8)
integer(8), parameter :: k(2,2) = reshape([h, -h, 4294967296_8, 5_8], [2, 2])
real, parameter :: rm(2,2) = reshape([1.5, 2.5, 3.5, 4.5], [2, 2])
logical, parameter :: lm(2,2) = reshape([.true., .false., .false., .true.], [2, 2])
character(len=2), parameter :: cm(2,2) = reshape(["ab", "cd", "ef", "gh"], [2, 2])
integer, parameter :: z(0,3) = reshape([integer ::], [0, 3])

integer, parameter :: s(2,3) = cshift(m, 1, dim=2)
integer, parameter :: c1(2,3) = cshift(m, 1)
integer, parameter :: c2(2,3) = cshift(m, -1, dim=2)
integer, parameter :: c3(2,3) = cshift(m, 7, dim=2)
integer, parameter :: c4(2,3) = cshift(m, -5, dim=1)
integer, parameter :: c5(2,3) = cshift(m, [1, 2, 3], dim=1)
integer, parameter :: c6(2,3) = cshift(m, [1, -1], dim=2)
integer, parameter :: c7(2,2,2) = cshift(t, 1, dim=3)
integer, parameter :: e1(2,3) = eoshift(m, 1, dim=2)
integer, parameter :: e2(2,3) = eoshift(m, -1)
integer, parameter :: e3(2,3) = eoshift(m, 4, boundary=-7, dim=2)
integer, parameter :: e4(2,3) = eoshift(m, [1, -1], boundary=[8, 9], dim=2)
integer, parameter :: e5(2,3) = eoshift(m, [-1, 0, 3], boundary=[1, 2, 3], dim=1)
integer, parameter :: e6(2,2,2) = eoshift(t, reshape([1, -1, 0, 1], [2, 2]), dim=2)
integer(8), parameter :: k1(2,2) = cshift(k, 1, dim=2)
integer(8), parameter :: k2(2,2) = eoshift(k, -1, boundary=3000000000_8)
real, parameter :: r1(2,2) = cshift(rm, 1, dim=2)
real, parameter :: r2(2,2) = eoshift(rm, -1, boundary=9.5)
logical, parameter :: l1(2,2) = cshift(lm, 1)
logical, parameter :: l2(2,2) = eoshift(lm, 1, dim=2)
character(len=2), parameter :: ch1(2,2) = cshift(cm, 1, dim=2)
character(len=2), parameter :: ch2(2,2) = eoshift(cm, 1)
character(len=2), parameter :: ch3(2,2) = eoshift(cm, -1, boundary="zz", dim=2)
integer, parameter :: z1(0,3) = cshift(z, 1, dim=2)
integer, parameter :: z2(0,3) = eoshift(z, [integer ::], dim=2)

if (any(s(:,1) /= [30, 40])) error stop 1
if (any(s(:,3) /= [10, 20])) error stop 2
if (any(s /= reshape([30, 40, 50, 60, 10, 20], [2, 3]))) error stop 3
if (any(c1 /= reshape([20, 10, 40, 30, 60, 50], [2, 3]))) error stop 4
if (any(c2 /= reshape([50, 60, 10, 20, 30, 40], [2, 3]))) error stop 5
if (any(c3 /= reshape([30, 40, 50, 60, 10, 20], [2, 3]))) error stop 6
if (any(c4 /= reshape([20, 10, 40, 30, 60, 50], [2, 3]))) error stop 7
if (any(c5 /= reshape([20, 10, 30, 40, 60, 50], [2, 3]))) error stop 8
if (any(c6 /= reshape([30, 60, 50, 20, 10, 40], [2, 3]))) error stop 9
if (any(c7 /= reshape([5, 6, 7, 8, 1, 2, 3, 4], [2, 2, 2]))) error stop 10
if (any(e1 /= reshape([30, 40, 50, 60, 0, 0], [2, 3]))) error stop 11
if (any(e2 /= reshape([0, 10, 0, 30, 0, 50], [2, 3]))) error stop 12
if (any(e3 /= -7)) error stop 13
if (any(e4 /= reshape([30, 9, 50, 20, 8, 40], [2, 3]))) error stop 14
if (any(e5 /= reshape([1, 10, 30, 40, 3, 3], [2, 3]))) error stop 15
if (any(e6 /= reshape([3, 0, 0, 2, 5, 8, 7, 0], [2, 2, 2]))) error stop 16
if (any(k1 /= reshape([4294967296_8, 5_8, h, -h], [2, 2]))) error stop 17
if (any(k2 /= reshape([3000000000_8, h, 3000000000_8, 4294967296_8], [2, 2]))) error stop 18
if (any(r1 /= reshape([3.5, 4.5, 1.5, 2.5], [2, 2]))) error stop 19
if (any(r2 /= reshape([9.5, 1.5, 9.5, 3.5], [2, 2]))) error stop 20
if (any(l1 .neqv. reshape([.false., .true., .true., .false.], [2, 2]))) error stop 21
if (any(l2 .neqv. reshape([.false., .true., .false., .false.], [2, 2]))) error stop 22
if (any(ch1 /= reshape(["ef", "gh", "ab", "cd"], [2, 2]))) error stop 23
if (any(ch2 /= reshape(["cd", "  ", "gh", "  "], [2, 2]))) error stop 24
if (any(ch3 /= reshape(["zz", "zz", "ab", "cd"], [2, 2]))) error stop 25
if (size(z1) /= 0 .or. size(z2) /= 0) error stop 26
print *, "ok"
end program
