program intrinsics_25
implicit none

integer(4), parameter:: x = ishft(123, 5)
integer(4), parameter:: y = ishft(25, 0)
integer(8), parameter:: z = ishft(1000000000, -2)
integer(4), parameter:: i1 = ishft(-1921, -4)
integer(8), parameter:: i2 = ishft(-6_8, -32_8)
integer(8), parameter:: i3 = ishft( 29382382013459_8, 12_8)
integer(8), parameter:: i4 = ishft(-987236238948_8, -23_8)
integer(1), parameter:: i5 = ishft(3, -4)
integer(1), parameter:: i6 = ishft(-4, 2)
integer(1), parameter:: i7 = ishft(123, -3)
integer(1), parameter:: i8 = ishft(-16, 1)
integer(2), parameter:: i9 = ishft(3, -4)
integer(2), parameter:: i10 = ishft(-4, 2)
integer(2), parameter:: i11 = ishft(123, -3)
integer(8) :: a1, a2, a3, a4
integer(4) :: b1, b2, b3, b4

a1 = -1921
a2 = -6_8
a3 = 29382382013459_8
a4 = -987236238948_8

a1 = ishft(a1, -4)
a2 = ishft(a2, -32_8)
a3 = ishft(a3, 12_8)
a4 = ishft(a4, -23_8)

b1 = -1921
b2 = -6
b3 = 29382382
b4 = -9872362

b1 = ishft(b1, -4)
b2 = ishft(b2, -31)
b3 = ishft(b3, 12)
b4 = ishft(b4, -23)

print *, b1, b2, b3, b4
print *, a1, a2, a3, a4
print *, i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11
print *, x, y, z

if (x /= 3936 .or. y /= 25 .or. z /= 250000000) error stop
if (a1 /= 1152921504606846855_8 .or. a2 /= 4294967295_8 .or. &
    a3 /= 120350236727128064_8 .or. a4 /= 2199023137864_8) error stop
if (b1 /= 268435335 .or. b2 /= 1 .or. &
    b3 /= 91152384 .or. b4 /= 510) error stop
if (i1 /= 268435335_8 .or. i2 /= 4294967295_8 .or. &
    i3 /= 120350236727128064_8 .or. i4 /= 2199023137864_8 .or. &
    i5 /= 0 .or. i6 /= -16 .or. i7 /= 15 .or. i8 /= -32 .or. &
    i9 /= 0 .or. i10 /= -16 .or. i11 /= 15) error stop

end program intrinsics_25