program real16_parameter_findloc_01
! findloc of a real(16) PARAMETER array with an integer value folds
! exactly in a PARAMETER initializer
implicit none
integer, parameter :: qp = 16
real(qp), parameter :: a(4) = [1.0_qp, 3.0_qp, 2.0_qp, 3.0_qp]
! 2**60 + 1 is not representable in double precision, but is in real(16)
real(qp), parameter :: b(3) = [1152921504606846976.0_qp, &
    1152921504606846977.0_qp, 0.0_qp]
integer, parameter :: i1 = findloc(a, 2, 1)
integer, parameter :: i2(1) = findloc(a, 3)
integer, parameter :: i3(1) = findloc(a, 3, back=.true.)
integer, parameter :: i4(1) = findloc(a, 7)
integer, parameter :: i5(1) = findloc(b, 1152921504606846977_8)
integer, parameter :: n = 2
integer, parameter :: i6 = findloc(a, n, 1)
if (i1 /= 3) error stop 1
if (i2(1) /= 2) error stop 2
if (i3(1) /= 4) error stop 3
if (i4(1) /= 0) error stop 4
if (i5(1) /= 2) error stop 5
if (i6 /= 3) error stop 6
print *, "ok"
end program
