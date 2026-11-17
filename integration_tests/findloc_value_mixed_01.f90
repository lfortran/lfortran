program findloc_value_mixed_01
! findloc compares ARRAY elements with VALUE as `array_elem == value` does
! (F2018 16.9.82, 10.1.5.5.1 and Table 10.2): both operands are converted
! to the type and kind of `array_elem + value`, never VALUE to the type or
! kind of ARRAY. Not labeled gfortran: GFortran converts VALUE to ARRAY's
! type and kind at run time and folds mixed-type PARAMETER cases wrongly.
implicit none
integer, parameter :: ip(4) = [1, 2, 3, 2]
real(4), parameter :: x4p(3) = [0.5, 0.1, 2.0]
complex(4), parameter :: z4p(3) = [(0.5,0.0), (0.1,0.0), (2.0,0.0)]
integer, parameter :: p1(1) = findloc(ip, 2.5)
integer, parameter :: p2(1) = findloc(ip, 2.0, back=.true.)
integer, parameter :: p3(1) = findloc(ip, 4294967298_8)
integer, parameter :: p4(1) = findloc(x4p, 0.1d0)
integer, parameter :: p5(1) = findloc(z4p, 0.1d0)
integer, parameter :: p6(1) = findloc(ip, (2.0,0.0))
integer, parameter :: p7(1) = findloc(ip, (2.0,1.0))
integer, parameter :: p8(1) = findloc(x4p, 2)
integer, parameter :: p9(1) = findloc(z4p, 2)
integer, parameter :: p10(1) = findloc(x4p, (2.0,1.0))
integer(4) :: i4(4)
real(4) :: x4(3)
complex(4) :: z4(3)
real(8) :: r8
integer(8) :: n8
i4 = ip; x4 = x4p; z4 = z4p
! an integer array never matches a non-integral real value
if (any(findloc(i4, 2.5) /= 0)) error stop 1
if (any(findloc(i4, 2.5d0, back=.true.) /= 0)) error stop 2
if (findloc(i4, 2.5, dim=1) /= 0) error stop 3
if (any(findloc(i4, 2.0, back=.true.) /= 4)) error stop 4
! integer(8) value outside the integer(4) range is not truncated
n8 = 4294967298_8
if (any(findloc(i4, n8) /= 0)) error stop 5
if (any(findloc(i4, 4294967298_8) /= 0)) error stop 6
! a real(8) value is not narrowed to real(4)
r8 = 0.1d0
if (any(findloc(x4, r8) /= 0)) error stop 7
if (any(findloc(x4, 0.1d0) /= 0)) error stop 8
if (any(findloc(z4, r8) /= 0)) error stop 9
if (any(findloc(z4, (0.1d0,0d0)) /= 0)) error stop 10
if (any(findloc(x4, real(0.1, 8)) /= 2)) error stop 11
! a complex value with a nonzero imaginary part never matches a real array
if (any(findloc(i4, (2.0,1.0)) /= 0)) error stop 12
if (any(findloc(x4, (2.0,1.0)) /= 0)) error stop 13
! PARAMETER folding agrees with run time
if (p1(1) /= 0) error stop 21
if (p2(1) /= 4) error stop 22
if (p3(1) /= 0) error stop 23
if (p4(1) /= 0) error stop 24
if (p5(1) /= 0) error stop 25
if (p6(1) /= 2) error stop 26
if (p7(1) /= 0) error stop 27
if (p8(1) /= 3) error stop 28
if (p9(1) /= 3) error stop 29
if (p10(1) /= 0) error stop 30
print *, "ok"
end program
