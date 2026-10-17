program intrinsics_493
! dot_product, pack and unpack of kind=1 and kind=2 constant arrays
implicit none
integer(1), parameter :: a(3) = [1_1, 2_1, 3_1]
integer(2), parameter :: h(3) = [4_2, 5_2, 6_2]
logical, parameter :: m(3) = [.true., .false., .true.]
integer(1) :: pa(2), ua(3)
integer(2) :: ph(2), uh(3)

if (dot_product(a, a) /= 14_1) error stop
if (dot_product(h, h) /= 77_2) error stop

pa = pack(a, m)
if (any(pa /= [1_1, 3_1])) error stop
ph = pack(h, m)
if (any(ph /= [4_2, 6_2])) error stop

ua = unpack(a, m, 0_1)
if (any(ua /= [1_1, 0_1, 2_1])) error stop
uh = unpack(h, m, 0_2)
if (any(uh /= [4_2, 0_2, 5_2])) error stop

print *, dot_product(a, a), dot_product(h, h)
print *, pa, ph
print *, ua, uh
end program
