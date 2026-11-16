program cshift_eoshift_int8_fold_01
! cshift and eoshift of an integer(8) constant array keep all 64 bits of each
! element when folded in a PARAMETER initializer (#17880)
implicit none
integer(8), parameter :: h = huge(1_8)
integer(8), parameter :: a(4) = [h, -h, 4294967296_8, 5_8]
integer(8), parameter :: c1(4) = cshift(a, 1)
integer(8), parameter :: c2(4) = cshift(a, -1)
integer(8), parameter :: o1(4) = eoshift(a, 1)
integer(8), parameter :: o2(4) = eoshift(a, -1)
integer(8), parameter :: o3(4) = eoshift(a, 2, -h)
integer(8), parameter :: o4(4) = eoshift(a, -2, 7_8)

if (c1(1) /= -h .or. c1(2) /= 4294967296_8 .or. c1(3) /= 5_8 .or. c1(4) /= h) error stop 1
if (c2(1) /= 5_8 .or. c2(2) /= h .or. c2(3) /= -h .or. c2(4) /= 4294967296_8) error stop 2
if (o1(1) /= -h .or. o1(2) /= 4294967296_8 .or. o1(3) /= 5_8 .or. o1(4) /= 0_8) error stop 3
if (o2(1) /= 0_8 .or. o2(2) /= h .or. o2(3) /= -h .or. o2(4) /= 4294967296_8) error stop 4
if (o3(1) /= 4294967296_8 .or. o3(2) /= 5_8 .or. o3(3) /= -h .or. o3(4) /= -h) error stop 5
if (o4(1) /= 7_8 .or. o4(2) /= 7_8 .or. o4(3) /= h .or. o4(4) /= -h) error stop 6
print *, "ok"
end program
