program doloop_22
! DO loop semantics with bounds and steps known only at run time:
! iteration counts, zero-trip loops, negative and non-unit steps, the DO
! variable after completion and after `exit`, loop variables of every
! integer kind near the ends of their ranges, and bounds or steps changed
! inside the body (they do not change the iteration count).
implicit none
integer :: i, n, lo, hi, st, cnt, isum
integer(1) :: k1, lo1, hi1
integer(2) :: k2, lo2, hi2
integer(8) :: k8, lo8, hi8, cnt8
real :: x(1000), y(1000), s
character(len=32) :: buf

buf = "3 7 2"
read(buf, *) lo, hi, st

! unit step, runtime bounds
cnt = 0; isum = 0
do i = lo, hi
    cnt = cnt + 1
    isum = isum + i
end do
if (cnt /= 5) error stop 1
if (isum /= 25) error stop 2
if (i /= hi + 1) error stop 3

! zero-trip loop: the DO variable is set to the start value
cnt = 0
do i = hi, lo
    cnt = cnt + 1
end do
if (cnt /= 0) error stop 4
if (i /= hi) error stop 5

! negative unit step
cnt = 0; isum = 0
do i = hi, lo, -1
    cnt = cnt + 1
    isum = isum + i
end do
if (cnt /= 5) error stop 6
if (isum /= 25) error stop 7
if (i /= lo - 1) error stop 8

! zero-trip loop with a negative step
cnt = 0
do i = lo, hi, -1
    cnt = cnt + 1
end do
if (cnt /= 0) error stop 9
if (i /= lo) error stop 10

! non-unit runtime steps
cnt = 0; isum = 0
do i = lo, hi, st
    cnt = cnt + 1
    isum = isum + i
end do
if (cnt /= 3) error stop 11
if (isum /= 15) error stop 12
if (i /= 9) error stop 13
cnt = 0; isum = 0
do i = hi, lo, -st
    cnt = cnt + 1
    isum = isum + i
end do
if (cnt /= 3) error stop 14
if (isum /= 15) error stop 15
if (i /= 1) error stop 16

! exit and cycle
cnt = 0
do i = lo, hi
    if (i == 4) cycle
    cnt = cnt + 1
    if (i == 6) exit
end do
if (cnt /= 3) error stop 17
if (i /= 6) error stop 18
cnt = 0
do i = hi, lo, -1
    cnt = cnt + 1
    if (i == 5) exit
end do
if (cnt /= 3) error stop 19
if (i /= 5) error stop 20

! bounds and step changed inside the body do not change the count
n = hi
cnt = 0
do i = lo, n
    n = n - 1
    cnt = cnt + 1
end do
if (cnt /= 5) error stop 21
if (n /= hi - 5) error stop 22
st = 1
cnt = 0
do i = lo, hi, st
    st = st + 1
    cnt = cnt + 1
end do
if (cnt /= 5) error stop 23

! integer(1) loop variables at both ends of the range
lo1 = int(-huge(lo) / huge(lo) - 127, 1)    ! -128
hi1 = int(huge(lo) / huge(lo) + 125, 1)     !  126
cnt = 0
do k1 = lo1, lo1 + 3_1
    cnt = cnt + 1
end do
if (cnt /= 4) error stop 24
if (k1 /= -124_1) error stop 25
cnt = 0
do k1 = hi1 - 3_1, hi1
    cnt = cnt + 1
end do
if (cnt /= 4) error stop 26
if (k1 /= 127_1) error stop 27
cnt = 0
do k1 = hi1 + 1_1, hi1 - 2_1, -1_1
    cnt = cnt + 1
end do
if (cnt /= 4) error stop 28
if (k1 /= 123_1) error stop 29
cnt = 0
do k1 = lo1 + 3_1, lo1 + 1_1, -1_1
    cnt = cnt + 1
end do
if (cnt /= 3) error stop 30
if (k1 /= lo1) error stop 31

! constant starts at and next to the ends of the integer(1) range
cnt = 0
do k1 = -127_1, lo1 + 3_1
    cnt = cnt + 1
end do
if (cnt /= 3) error stop 50
if (k1 /= -124_1) error stop 51
cnt = 0
do k1 = -huge(k1) - 1_1, lo1 + 1_1
    cnt = cnt + 1
end do
if (cnt /= 2) error stop 52
if (k1 /= -126_1) error stop 53
cnt = 0
do k1 = 126_1, hi1 - 1_1, -1_1
    cnt = cnt + 1
end do
if (cnt /= 2) error stop 54
if (k1 /= 124_1) error stop 55
cnt = 0
do k1 = 127_1, hi1, -1_1
    cnt = cnt + 1
end do
if (cnt /= 2) error stop 56
if (k1 /= 125_1) error stop 57

! zero-trip loops with a constant start
cnt = 0
do i = 5, lo
    cnt = cnt + 1
end do
if (cnt /= 0) error stop 58
if (i /= 5) error stop 59
cnt = 0
do i = 1, hi, -1
    cnt = cnt + 1
end do
if (cnt /= 0) error stop 60
if (i /= 1) error stop 61

! integer(2) loop variables at both ends of the range
lo2 = -huge(lo2) - 1_2
hi2 = huge(hi2)
lo2 = lo2 + int(lo - 3, 2)
cnt = 0
do k2 = lo2, lo2 + 9_2
    cnt = cnt + 1
end do
if (cnt /= 10) error stop 32
if (k2 /= -huge(k2) + 9_2) error stop 33
cnt = 0
do k2 = hi2 + int(lo - 3, 2), hi2 - 9_2, -1_2
    cnt = cnt + 1
end do
if (cnt /= 10) error stop 34

! integer(8) loop variables beyond the integer(4) range
lo8 = -huge(lo8) - 1_8 + int(lo - 3, 8)
hi8 = huge(hi8) - 1_8
cnt8 = 0
do k8 = lo8, lo8 + 4_8
    cnt8 = cnt8 + 1
end do
if (cnt8 /= 5_8) error stop 35
if (k8 /= lo8 + 5_8) error stop 36
cnt8 = 0
do k8 = hi8 - 4_8, hi8
    ! The delta is 1..5; adding k8 before subtracting would overflow.
    cnt8 = cnt8 + (k8 - (hi8 - 5_8))
end do
if (cnt8 /= 15_8) error stop 37
if (k8 /= huge(k8)) error stop 38
cnt8 = 0
do k8 = int(huge(lo), 8) - 1_8, int(huge(lo), 8) + 2_8
    cnt8 = cnt8 + 1
end do
if (cnt8 /= 4_8) error stop 39

! a reduction over runtime-size arrays
n = 1000
do i = 1, n
    x(i) = real(i)
    y(i) = 2
end do
read(buf, *) lo, hi
n = 250 * lo + 250
s = 0
do i = 1, n
    s = s + x(i) * y(i)
end do
if (abs(s - 1001000.0) > 1.0) error stop 40
s = 0
do i = n, 1, -1
    s = s + x(i)
end do
if (abs(s - 500500.0) > 1.0) error stop 41
print *, "ok"
end program
