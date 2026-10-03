program bits_07
! Test bgt/blt/bge/ble with int64 values > 2^32 (overflow-safe unsigned comparison)
implicit none
integer(8) :: a, b, c, d

a = 8589934591_8   ! 2^33 - 1
b = 4294967296_8   ! 2^32
c = -1_8           ! all bits set (largest unsigned)
d = 0_8

! bgt tests
print *, bgt(a, b)
if (.not. bgt(a, b)) error stop "bgt(2^33-1, 2^32) should be true"
print *, bgt(b, a)
if (bgt(b, a)) error stop "bgt(2^32, 2^33-1) should be false"
print *, bgt(c, a)
if (.not. bgt(c, a)) error stop "bgt(-1, 2^33-1) should be true"
print *, bgt(a, c)
if (bgt(a, c)) error stop "bgt(2^33-1, -1) should be false"
print *, bgt(a, a)
if (bgt(a, a)) error stop "bgt(x, x) should be false"
print *, bgt(c, d)
if (.not. bgt(c, d)) error stop "bgt(-1, 0) should be true"
print *, bgt(d, c)
if (bgt(d, c)) error stop "bgt(0, -1) should be false"

! blt tests
print *, blt(b, a)
if (.not. blt(b, a)) error stop "blt(2^32, 2^33-1) should be true"
print *, blt(a, b)
if (blt(a, b)) error stop "blt(2^33-1, 2^32) should be false"
print *, blt(a, c)
if (.not. blt(a, c)) error stop "blt(2^33-1, -1) should be true"
print *, blt(a, a)
if (blt(a, a)) error stop "blt(x, x) should be false"

! bge tests
print *, bge(a, b)
if (.not. bge(a, b)) error stop "bge(2^33-1, 2^32) should be true"
print *, bge(a, a)
if (.not. bge(a, a)) error stop "bge(x, x) should be true"
print *, bge(b, a)
if (bge(b, a)) error stop "bge(2^32, 2^33-1) should be false"
print *, bge(c, a)
if (.not. bge(c, a)) error stop "bge(-1, 2^33-1) should be true"

! ble tests
print *, ble(b, a)
if (.not. ble(b, a)) error stop "ble(2^32, 2^33-1) should be true"
print *, ble(a, a)
if (.not. ble(a, a)) error stop "ble(x, x) should be true"
print *, ble(a, b)
if (ble(a, b)) error stop "ble(2^33-1, 2^32) should be false"
print *, ble(a, c)
if (.not. ble(a, c)) error stop "ble(2^33-1, -1) should be true"

end program
