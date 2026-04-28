program format_101
! Test that D/E format correctly adjusts the exponent when rounding
! the mantissa causes a carry (e.g., 0.96 rounds to 1.0).
implicit none
character(len=20) :: s
double precision :: x

! 0.095d0 with D9.1: mantissa 0.95 rounds to 1.0, exponent must increase
x = 0.095d0
write(s, '(D9.1)') x
print *, "D9.1:", s
if (trim(adjustl(s)) /= "0.1D+00") error stop

! 0.96d0 with D9.1: mantissa rounds up, exponent must increase
x = 0.96d0
write(s, '(D9.1)') x
print *, "D9.1:", s
if (trim(adjustl(s)) /= "0.1D+01") error stop

! 0.996d0 with E9.2: mantissa rounds causing carry
x = 0.996d0
write(s, '(E9.2)') x
print *, "E9.2:", s
if (trim(adjustl(s)) /= "0.10E+01") error stop

! No rounding carry case: should still work
x = 0.5d0
write(s, '(D9.1)') x
print *, "D9.1:", s
if (trim(adjustl(s)) /= "0.5D+00") error stop

! Negative value with rounding carry
x = -0.096d0
write(s, '(D10.1)') x
print *, "D10.1:", s
if (trim(adjustl(s)) /= "-0.1D+00") error stop

! Larger value with rounding carry
x = 9.96d0
write(s, '(E9.2)') x
print *, "E9.2:", s
if (trim(adjustl(s)) /= "0.10E+02") error stop

print *, "All tests passed!"
end program
