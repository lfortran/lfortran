program gpu_metal_89
! Test that do concurrent with 31+ arrays works correctly.
! Metal limits buffer indices to 0-30, so the compiler must pack arrays
! into a combined buffer when the count exceeds this limit.
implicit none
integer, parameter :: n = 10
real :: a1(n), a2(n), a3(n), a4(n), a5(n), a6(n), a7(n), a8(n)
real :: a9(n), a10(n), a11(n), a12(n), a13(n), a14(n), a15(n), a16(n)
real :: a17(n), a18(n), a19(n), a20(n), a21(n), a22(n), a23(n), a24(n)
real :: a25(n), a26(n), a27(n), a28(n), a29(n), a30(n), a31(n)
integer :: i

a2 = 1.0; a3 = 1.0; a4 = 1.0; a5 = 1.0; a6 = 1.0; a7 = 1.0; a8 = 1.0
a9 = 1.0; a10 = 1.0; a11 = 1.0; a12 = 1.0; a13 = 1.0; a14 = 1.0; a15 = 1.0
a16 = 1.0; a17 = 1.0; a18 = 1.0; a19 = 1.0; a20 = 1.0; a21 = 1.0; a22 = 1.0
a23 = 1.0; a24 = 1.0; a25 = 1.0; a26 = 1.0; a27 = 1.0; a28 = 1.0; a29 = 1.0
a30 = 1.0; a31 = 1.0

do concurrent(i = 1:n)
  a1(i) = a2(i) + a3(i) + a4(i) + a5(i) + a6(i) + a7(i) + a8(i) &
       + a9(i) + a10(i) + a11(i) + a12(i) + a13(i) + a14(i) + a15(i) &
       + a16(i) + a17(i) + a18(i) + a19(i) + a20(i) + a21(i) + a22(i) &
       + a23(i) + a24(i) + a25(i) + a26(i) + a27(i) + a28(i) + a29(i) &
       + a30(i) + a31(i)
end do

print *, a1(1)
if (abs(a1(1) - 30.0) > 1e-6) error stop
if (abs(a1(n) - 30.0) > 1e-6) error stop
end program
