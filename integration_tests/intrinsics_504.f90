program intrinsics_504
! ishft, shiftl, shiftr and shifta with |shift| == bit_size (#16451)
implicit none
integer(1), parameter :: c1 = shiftr(-1_1, 8) + shifta(-1_1, 8) + shiftl(-1_1, 8)
integer(4), parameter :: c4(6) = [ishft(1, 32), ishft(-1, -32), shiftl(-1, 32), &
    shiftr(-1, 32), shifta(1, 32), shifta(-1, 32)]
integer(8), parameter :: c8(6) = [ishft(1_8, 64), ishft(-1_8, -64), &
    shiftl(-1_8, 64), shiftr(-1_8, 64), shifta(1_8, 64), shifta(-1_8, 64)]
integer, parameter :: a1(2) = shifta([-8, 8], 2)
integer, parameter :: a2(2) = shifta(-8, [1, 32])
integer(1), parameter :: a3(2) = shifta([-8_1, 8_1], [1_8, 8_8])
integer(8), parameter :: a4(3) = shifta([-16_8, 16_8, -1_8], [2, 4, 64])
integer :: w(3), v(3)

if (c1 /= -1) error stop 1
if (any(c4 /= [0, 0, 0, 0, 0, -1])) error stop 2
if (any(c8 /= [0_8, 0_8, 0_8, 0_8, 0_8, -1_8])) error stop 3

call t1(8)
call t2(16)
call t4(32)
call t8(64)

w = [1, 31, 32]
if (any(shifta(-8, w) /= [-4, -1, -1])) error stop 4
if (any(shifta(8, w) /= [4, 0, 0])) error stop 5
if (any(shiftr(-1, w) /= [huge(0), 1, 0])) error stop 6
if (any(shiftl(1, w) /= [2, -huge(0) - 1, 0])) error stop 7
if (any(a1 /= [-2, 2])) error stop 8
if (any(a2 /= [-4, -1])) error stop 9
if (any(a3 /= [-4_1, 0_1])) error stop 10
if (any(a4 /= [-4_8, 1_8, -1_8])) error stop 30
v = [-8, 8, -1]
if (any(shifta(v, 2) /= [-2, 2, -1])) error stop 31
if (any(shifta(v, w) /= [-4, 0, -1])) error stop 32
if (any(shifta(i=v, shift=32) /= [-1, 0, -1])) error stop 33
print *, "ok"

contains

subroutine t1(n)
integer, intent(in) :: n
integer(1) :: a, b
a = 1
b = -1
if (ishft(a, n) /= 0) error stop 11
if (ishft(b, -n) /= 0) error stop 12
if (shiftl(b, n) /= 0) error stop 13
if (shiftr(b, n) /= 0) error stop 14
if (shifta(a, n) /= 0) error stop 15
if (shifta(b, n) /= -1) error stop 16
if (ishft(b, n - 1) /= -huge(a) - 1) error stop 17
if (shifta(b, n - 1) /= -1) error stop 18
end subroutine

subroutine t2(n)
integer, intent(in) :: n
integer(2) :: a, b
a = 1
b = -1
if (ishft(a, n) /= 0) error stop 21
if (ishft(b, -n) /= 0) error stop 22
if (shiftl(b, n) /= 0) error stop 23
if (shiftr(b, n) /= 0) error stop 24
if (shifta(a, n) /= 0) error stop 25
if (shifta(b, n) /= -1) error stop 26
if (ishft(b, n - 1) /= -huge(a) - 1) error stop 27
if (shiftr(b, n - 1) /= 1) error stop 28
end subroutine

subroutine t4(n)
integer, intent(in) :: n
integer(4) :: a, b
a = 1
b = -1
if (ishft(a, n) /= 0) error stop 41
if (ishft(b, -n) /= 0) error stop 42
if (shiftl(b, n) /= 0) error stop 43
if (shiftr(b, n) /= 0) error stop 44
if (shifta(a, n) /= 0) error stop 45
if (shifta(b, n) /= -1) error stop 46
if (ishft(b, n - 1) /= -huge(a) - 1) error stop 47
if (shiftr(b, n - 1) /= 1) error stop 48
end subroutine

subroutine t8(n)
integer, intent(in) :: n
integer(8) :: a, b
a = 1
b = -1
if (ishft(a, n) /= 0) error stop 81
if (ishft(b, -n) /= 0) error stop 82
if (shiftl(b, n) /= 0) error stop 83
if (shiftr(b, n) /= 0) error stop 84
if (shifta(a, n) /= 0) error stop 85
if (shifta(b, n) /= -1) error stop 86
if (ishft(b, n - 1) /= -huge(a) - 1) error stop 87
if (shiftr(b, n - 1) /= 1) error stop 88
end subroutine

end program
